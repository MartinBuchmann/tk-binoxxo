;;;; tk-binoxxo.lisp
;;; Time-stamp: <2017-01-26 21:38:52 Martin>
;;;
;;; Die Ausgabe per TK-Widgets. Das eigentliche Lösen erfolgt der gesonderten Engine.
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

(in-package :tk-binoxxo)

;;; Die wichtigsten Parameter für die Darstellung
(defparameter *feldgröße* 480)
(defparameter *versatz* 5)
(defvar *größe*)
(defvar *abstand*)

;;; Intialisiert die Größe des Feldes
(setf *größe* 6)
(setf *abstand* (/ *feldgröße* *größe*))

;;; Liest ein Rätsel aus einer externen Datei
(defun lies-rätsel (datei größe spiel)
  "Liest ein binoxxo-Rätsel mit Größe x Größe aus einer DATEI in das Array
SPIEL ein. Dabei ist eine Zeile ein Rätsel."
  (with-open-file (s datei :direction :input :if-does-not-exist :error)
    (let ((input (read-line s)))
      (if (not (equal größe (isqrt (length input))))
	  (error "Größe: ~D enstpricht nicht der Länge des Spielfeldes: ~D!" größe (isqrt (length input)))
	  (loop
	    for index below (* größe größe) ; 
	    for digit = (cond ((equal #\X (char input index)) 1)
			      ((equal #\O (char input index)) 5)
			      (t 0))
	    do (setf (row-major-aref spiel index) digit))))))

(defun zeichne-spielfeld (spielfeld)
  (ltk:clear spielfeld)
  (loop for i from 1 below *größe*
	for line = (ltk:create-line* spielfeld
				     (+ *versatz* (* i *abstand*)) *versatz*
				     (+ *versatz* (* i *abstand*)) (+ *versatz* *feldgröße*))
	do  (ltk:itemconfigure spielfeld line :width 2))
  (loop for i from 1 below *größe*
	for line = (ltk:create-line* spielfeld
				     *versatz* (+ *versatz* (* i *abstand*))
				     (+ *versatz* *feldgröße*) (+ *versatz* (* i *abstand*)))
	do  (ltk:itemconfigure spielfeld line :width 2))
  (ltk:itemconfigure spielfeld (ltk:create-rectangle spielfeld
						     *versatz* *versatz*
						     (+ *versatz* *feldgröße*) (+ *versatz* *feldgröße*)) :width 3))

(defun zeichne-symbol (spielfeld symbol zeile spalte)
  ;; Empirisches Feintuning
  (cond ((string= symbol "X")
	 (ltk:itemconfigure spielfeld
			    (ltk:create-text spielfeld
					     (+ *versatz* (* *abstand* 0.27) (* *abstand* spalte))
					     (+ *versatz* (* *abstand* 0.05) (* *abstand* zeile))
					     symbol) :font (cond ((= *größe* 6) "Sans 60")
								 ((= *größe* 8) "Sans 42"))))
	((string= symbol "O")
	 (ltk:itemconfigure spielfeld
			    (ltk:create-text spielfeld
					     (+ *versatz* (* *abstand* 0.19) (* *abstand* spalte))
					     (+ *versatz* (* *abstand* 0.00) (* *abstand* zeile))
					     symbol) :font (cond ((= *größe* 6) "Sans 66")
								 ((= *größe* 8) "Sans 44"))))))

;;; Konvertiert die Zahlen zu Buchstaben
(defun felder-konvertieren (v)
  "Konvertiert die numerischen Werte 0, 1, 5 in \" \", X und O."
  (cond ((equal v 1)  "X")
	((equal v 5)  "O")
	(t " ")))

;;; Gibt das aktuelle Spielfeld aus
(defun gib-feld-aus (spiel spielfeld)
  "Gibt das aktuelle Spielfeld SPIEL aus."
  ;; Zeichnet ein leeres Spielfeld
  (zeichne-spielfeld spielfeld)
  (let ((größe (array-dimension spiel 0)))
  (loop for i below größe
	do (loop for j below größe
		 do (zeichne-symbol spielfeld (felder-konvertieren (aref spiel i j)) i j)))))


(defun tk-binoxxo ()
  (let ((spiel (make-array (list *größe* *größe*) :initial-element 0 :adjustable t)))
   (ltk:with-ltk ()
      (let* ((vals (list 6 8))
	     (feld (make-instance 'ltk:frame))
	     (steuerung (make-instance 'ltk:frame :master feld))
	     (spielfeld (ltk:make-canvas feld
					 :width (+ *versatz* *feldgröße*)
					 :height (+ *versatz* *feldgröße*)))
	     (spinbox
	       (make-instance 'ltk:spinbox
			      :width 3
			      :command (lambda (val)
					 (setf *größe* (parse-integer val))
					 (setf *abstand* (/ *feldgröße* *größe*))
					 (zeichne-spielfeld spielfeld)
					 (adjust-array spiel (list *größe* *größe*) :initial-element 0)
					 )
			      :master steuerung
			      :values vals))
	     (lösen (make-instance 'ltk:button 
				   :master steuerung
			     :text "Lösen"
				   :command (lambda ()
					      (gib-feld-aus (lösen spiel) spielfeld)
					      )))
	     (erstellen (make-instance 'ltk:button 
				       :master steuerung
				       :text "Erstellen"
				       :command (lambda ()
						  (setf spiel (erstelle-rätsel *größe*))
						  (gib-feld-aus  spiel spielfeld)
						  )))
	     (datei (make-instance 'ltk:button
				   :master steuerung
				   :text "Datei"
				   :command (lambda ()
					      (setf spiel (make-array (list *größe* *größe*)
								      :initial-element 0))
					      (lies-rätsel
					       (ltk:get-open-file :filetypes nil
								  :title "Öffne ein binoxxo...")
					       *größe* spiel)
					      (gib-feld-aus spiel spielfeld)))))
	;; Das Fenster mit den drei Steuerelementen
	;; und der Zeichnungsfläche
	(ltk:wm-title ltk:*tk* "binoxxo")
	(ltk:configure spielfeld :background :white)
	(ltk:pack feld)
	(ltk:pack spielfeld)
	(ltk:pack steuerung)
	(ltk:pack datei :side :left :expand t :padx 10 :pady 10)
	(ltk:pack spinbox :side :left :after datei :padx 10)
	(ltk:pack lösen :side :left :expand t :padx 10)
	(ltk:pack erstellen :side :left :expand t :padx 10)

	;; Abhängig von der Anzahl der Felder das Spielfeld zeichnen.
	(zeichne-spielfeld spielfeld)))))
