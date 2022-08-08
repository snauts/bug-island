(defparameter *save-picture* nil)

(declaim (ftype function get-fov-cells))

(defparameter *max-water-alt* nil)
(defparameter *max-land-alt* nil)

(defun adjacent-water (world c)
  (let ((*fov* 1))
    (get-fov-cells world c)))

(defun test-height (i)
  (lambda (x) (and (numberp (cell-alt x)) (= (1- i) (cell-alt x)))))

(defun fill-alt (world)
  (setf *max-land-alt* 0.0)
  (setf *max-water-alt* 0)
  (dotimes (i (max (map-width) (map-height)))
    (for-each-cell
     world
     (lambda (c)
       (when (and (is-land c) (null (cell-alt c)))
	 (let ((n (adjacent-cells c 1)))
	   (cond ((< (length n) 4)
		  (setf (cell-alt c) 0))
		 ((member-if (test-height i) n)
		  (when (> i *max-land-alt*)
		    (setf *max-land-alt* i))
		  (setf (cell-alt c) i)))))
       (when (and (is-water c) (null (cell-alt c)))
	 (let ((n (adjacent-water world c)))
	   (cond ((member-if #'is-land n)
		  (setf (cell-alt c) 0))
		 ((member-if (test-height i) n)
		  (when (> i *max-water-alt*)
		    (setf *max-water-alt* i))
		  (setf (cell-alt c) i)))))))))

(defvar *out* nil)

(defun file-name ()
  (format nil "pic-~5,'0d.pnm" *epoch*))

(defun save-color (r g b)
  (format *out* "~A~%~A~%~A~%" r g b))

(defun scale-color (c q)
  (mapcar (lambda (x) (* x q)) c))

(defun blend-color (c1 c2 q)
  (mapcar (lambda (x1 x2) (+ (* x1 q) (* x2 (- 1.0 q)))) c1 c2))

(defun save-color-float (c)
  (apply #'save-color (mapcar (lambda (x) (floor (* 255 x))) c)))

(defparameter *grazer-color* '(1.0 0.0 0.0))
(defparameter *predator-color* '(1.0 0.0 1.0))

(defun intensity (b)
  (+ 0.25 (* 0.5 (/ (bug-size b) *max-size*))))

(defun save-grazer-pixel (i)
  (save-color-float (scale-color *grazer-color* i)))

(defun save-predator-pixel (i)
  (save-color-float (scale-color *predator-color* i)))

(defun save-bug-pixel (b)
  (if (is-grazer b)
      (save-grazer-pixel (intensity b))
      (save-predator-pixel (intensity b))))

(defparameter *forest-color* '(0.00 0.75 0.00))
(defparameter *desert-color* '(0.25 0.25 0.00))

(defun save-land-pixel (c)
  (let ((a (+ 0.5 (* 0.5 (/ (cell-alt c) *max-land-alt*)))))
    (save-color-float
     (blend-color
      (scale-color *forest-color* a)
      (scale-color *desert-color* a)
      (/ (cell-food c) *max-food*)))))

(defparameter *water-color* '(0 0.375 0.5))

(defun save-water-pixel (w)
  (let ((q (- 1.0 (* 0.8 (/ w *max-water-alt*)))))
    (save-color-float (scale-color *water-color* q))))

(defun save-cell-pixel (c)
  (cond ((is-occupied c) (save-bug-pixel (cell-bug c)))
	((is-water c) (save-water-pixel (cell-alt c)))
	((is-land c) (save-land-pixel c))
	(t (save-color 0 0 0))))

(defun save-picture (world)
  (with-open-file (*out* (file-name) :direction :output :if-exists :supersede)
    (format *out* "P3~%~A ~A 255~%" (map-width) (map-height))
    (for-each-cell world (lambda (c) (save-cell-pixel c)))))
