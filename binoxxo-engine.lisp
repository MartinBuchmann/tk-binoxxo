;;;; binoxxo-engine.lisp
;;; Time-stamp: <2017-01-26 21:38:23 Martin>
;;;
;;; Der grundlegende Algorithmus zum Lösen von binoxxo-Rätseln mit 6x6 und 8x8 Feldern.
;;; Die Ausgabe erfolgt per TK widget und ist extern implementiert.
;;; Siehe z.B. http://www.kreuzwortraetsel.ch/techniken-binoxxo/
;;;
;;; Regeln:
;;; 1. In jeder Reihe müssen gleich viel X und O sein.
;;; 2. Höchstens zwei aufeinanderfolgende X und O.
;;; 3. Alle Reihen (Zeile und Spalten) sind einzigartig.
;;;
;;; Das Rätsel wird per Datei eingeben, ein Rätsel pro Zeile.
;;; Leere Felder werden durch "_" kodiert.
;;; Intern werden die Werte " " als 0, "X" als 1 und "O" als 5 kodiert.

(in-package #:binoxxo-engine)

;;; Parallel zur grafischen Ausgabe werden die einzelnen Schritte im repl ausgegeben.
;;; Die Spalten- und Zeilennamen
(defparameter spalten-namen '(A B C D E F G H)  "Die Spaltennamen")
(defparameter zeilen-nummern '(1 2 3 4 5 6 7 8) "Die Zeilennummern")

;;; Konvertiert die Zahlen zu Buchstaben
(defun felder-konvertieren (v)
  "Konvertiert die numerischen Werte 0, 1, 5 in \" \", X und O."
  (cond ((equal v 1)  "X")
	((equal v 5)  "O")
	(t " ")))

;;; Gibt das aktuelle Spielfeld aus
(defun gib-feld-aus (spiel i)
  "Gibt das aktuelle Spielfeld SPIEL aus. Abhänging von Schritt I wird die Überschrift gewählt."
  (let ((größe (array-dimension spiel 0)))
    (cond ((equal i 0) (format t "~&Ausgangsstellung:~%"))
	  ((numberp i) (format t "~%~% Runde: ~D~%" i))
	  ((equal i 'Rätsel) (format t "~%Neues Rätsel~%"))
	  (t (format t "~&Endstellung:~%")))
  (format t "~&   ~{~#[~;  ~A~%~:;  ~A ~]~}" (subseq spalten-namen 0 größe))
  (format t "    ~v@{~A~:*~}" (1- (* 4 größe)) "-")
  (loop 
     for i below größe
     do (format t "~& ~D " (1+ i))
       (loop 
	   for j below größe
	   do (format t "| ~D " (felder-konvertieren (aref spiel i j))))
       (format t "|~%"))
    (format t "    ~v@{~A~:*~}" (1- (* 4 größe)) "-")
    (fresh-line)))


;;; Ein Zeile des Spielfelds als Liste zurückgeben
(defun zeile (spiel zeile)
  "Ein Zeile des Spielfelds als Liste zurückgeben."
  (when (< zeile (array-dimension spiel 0))
    (loop for i below (array-dimension spiel 1)
	  collect (aref spiel zeile i))))

;;; Eine Spalte des Spielfeldes als Liste zurückgeben
(defun spalte (spiel spalte)
  "Eine Spalte des Spielfeldes als Liste zurückgeben"
  (when (< spalte (array-dimension spiel 1))
    (loop for i below (array-dimension spiel 0)
	  collect (aref spiel i spalte))))

;;; Anzahl freier Felder
(defun zähle-freie-felder (spiel)
  "Gibt die Anzahl der noch freien Felder zurück."
  (loop for i below (array-total-size spiel)
	count (equal 0 (row-major-aref spiel i))))

;;; Gibt es Tripel?
(defun tripel? (reihe)
  "Gibt T zurück wenn drei benachbarte Elemente gleich sind."
  (loop for i below (- (length reihe) 2)
	when (or (equal '(1 1 1) (subseq reihe i (+ i 3)))
		 (equal '(5 5 5) (subseq reihe i (+ i 3))))
	  return t))

;;; Gibt es drei gleiche Elemente in einer Reihe?
(defun keine-tripel? (spiel)
  "Gibt T zurück wenn keine Zeile oder Spalte ein Tripel enthält."
  (loop with größe = (array-dimension spiel 0)
	for i below größe
	for spalte = (spalte spiel i)
	for zeile  = (zeile spiel i)
	when (or (tripel? spalte) (tripel? zeile))
	  return nil
	finally (return t)))

;;; Prüfe, ob alle Zeilen und Spalten unterschiedlich sind
(defun doppeltep (spiel)
  "Prüft ob in einem Spiel doppelte Spalten oder Zeilen vorkommen. Gibt T
zurück, wenn es doppelte Zeilen oder Spalten gibt."
  (let* ((größe (array-dimension spiel 0))
	 (spalten	    ; Sammelt alle Spalten des Spielfeld in einer Liste
	   (loop for i below größe
		 collect (spalte spiel i)))
	 (zeilen	   ; Sammelt alle Zeilen des Spielfeldes in einer Liste
	   (loop for i below größe
	 	 collect (zeile spiel i))))
    (labels ((duplicates-p (lst)
	       (cond ((null lst) nil)
		     ((and
		       (zerop (count 0 (car lst)))
		       (member (car lst) (cdr lst) :test 'equal)) t)
		     (t (duplicates-p (rest lst))))))
      (or (duplicates-p zeilen) (duplicates-p spalten)))))

;;; Gibt es nur die erlaubte Anzahl von Werten pro Reihe?
(defun anzahl? (reihe)
  "Gibt T zurück wenn die maximale Anzahl von X und O größer als die Hälfte der 
Reihenlänge ist."
  (let ((maximum (/ (length reihe) 2)))
	(or (> (count 1 reihe) maximum)
	     (> (count 5 reihe) maximum))))

(defun korrekte-anzahl (spiel)
  "Prüft ob für alle Reihen die maximale Anzahl an X und O nicht überschritten ist."
  (loop with größe = (array-dimension spiel 0)
	for i below größe
	for spalte = (spalte spiel i)
	for zeile  = (zeile spiel i)
	when (or (anzahl? spalte) (anzahl? zeile))
	  return nil
	finally (return t)))
   
;;; Ist das Rätsel vollständig gelöst?
(defun rätsel-gelöst (spiel)
  (when (and
	 (zerop (zähle-freie-felder spiel)) ; Alle Felder sind belegt
	 (korrekte-anzahl spiel) ; Es gibt noch zu viele X oder O
	 (not (doppeltep spiel)) ; Alle Zeilen und Spalten sind verschieden
	 (keine-tripel? spiel)) ; Keine drei gleichen Elemente neben oder übereinander		     
    t ))

;;; Prüft, ob das unvollständige Spiel bislang erlaubt ist.
;;; Solange noch nicht alle Felder gefüllt sind, das einzige Kriterium
(defun valides-spiel (spiel)
  (when (and (korrekte-anzahl spiel)
	     (keine-tripel? spiel)
	     (not (doppeltep spiel)))
    t ))

(defun mögliche-spiele (spiel)
  "Nachfolgerfunktion für die nächsten möglichen Spiele zurück."
  (flet ((spiele (spiel i)
	   (let* ((möglichkeit-1 (copy-array spiel))
		  (möglichkeit-2 (copy-array spiel))
		  (wert1 (if (zerop (random 2)) 1 5)) ; Wir wollen zufällig aus 1 oder 5 auswählen
		  (wert2 (first (remove wert1 '(1 5))))) ; Wenn wert1 1 ist, dann 5 und umgekehrt.
	     ;; Es gibt hier nur die beiden möglichen Werte "1" und "5"
	     (setf (row-major-aref möglichkeit-1 i) wert1)
	     (setf (row-major-aref möglichkeit-2 i) wert2)
	     (remove nil (list (and (valides-spiel möglichkeit-1) möglichkeit-1)
			       (and (valides-spiel möglichkeit-2) möglichkeit-2))))))
    (loop with größe = (array-total-size spiel)
	  for i below größe
	  when (zerop (row-major-aref spiel i))
          return (spiele spiel i))))

;;; Suchen mittels depth-first-search
(defun suche-lösung (states goal-p successors)
  "Tiefensuche der möglichen Spielfelder (STATES)."
  (dbg :search "~&;; Search: ~a" (mapcar #'zähle-freie-felder states))
  (dbg :search "~&;; Search: ~S" (first states))
  (cond ((null states) nil)
        ((funcall goal-p (first states)) (first states))
        (t (suche-lösung
            (funcall #'append
                     (funcall successors (first states))
                     (rest states))
            goal-p successors))))

;;; Berechnet wie lange die Suche gedauert hat.
;;; Inspiriert von https://github.com/dimitri/sudoku
(defmacro timing (&body forms)
  "Return both how much real time was spend in body and its result"
  (let ((start (gensym))
	(end (gensym))
	(result (gensym)))
    `(let* ((,start (get-internal-real-time))
	    (,result (progn ,@forms))
	    (,end (get-internal-real-time)))
       (values ,result (/ (- ,end ,start) internal-time-units-per-second)))))

;;; Die Hauptroutine
(defun lösen (spiel)
  (gib-feld-aus spiel 0) ; Gibt die Ausgangsstellung aus.
  (if (valides-spiel spiel) ; Prüft, ob das Spiel zu lösen ist.
      (progn
	(format t "~&~3TLöse Rätsel mit ~D offenen Feldern...~%" (zähle-freie-felder spiel))
	(multiple-value-bind (Ergebnis Zeit)
	    (timing (suche-lösung (list spiel) #'rätsel-gelöst #'mögliche-spiele))
	  ;; Konnte das Puzzel gelöst werden?
	  (if Ergebnis
	      (progn
		(format t "~%~%Rätsel gelöst in ~2,3F Sekunden~%" Zeit)
		(gib-feld-aus Ergebnis t)
		Ergebnis)
	      (error "~&Keine Lösung gefunden!~%"))))
      (error "~&Kein gültiges binoxxo!")))

;;; Zusatzfunktion, die Rätsel erstellt
(defun erstelle-rätsel (größe)
  "Erstellt zufällige aber valide und lösbare Rätsel. Füllt ein leeres Feld und
entfernt dann einzelne Felder"
  (let* ((spiel (make-array (list größe größe) :initial-element 0))
	 (rätsel (suche-lösung (list spiel) #'rätsel-gelöst #'mögliche-spiele))
	 (leere-felder (max (floor (* größe (* 0.5 größe)))
			    (1+ (random (floor (* größe (* 0.8 größe)))))))) ; 50% < leer Felder < 80%
    ;; Setzt Felder 
    (loop until (= leere-felder (zähle-freie-felder rätsel))
	  for i = (random (* größe größe))
	  do
	     (setf (row-major-aref rätsel i) 0))
    (gib-feld-aus rätsel 'Rätsel)
    rätsel))
