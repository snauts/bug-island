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
  (format nil "pic.pnm"))

(defun save-color (r g b)
  (format *out* "~A~%~A~%~A~%" r g b))

(defun intensity (b)
  (let ((s (bug-size b)))
    (+ 55 (* 20 s))))

(defun save-grazer-pixel (i)
  (save-color i 0 0))

(defun save-predator-pixel (i)
  (save-color i 0 i))

(defun save-bug-pixel (b)
  (if (is-grazer b)
      (save-grazer-pixel (intensity b))
      (save-predator-pixel (intensity b))))

(defparameter *forest* '(0.00 0.75 0.00))
(defparameter *desert* '(0.25 0.25 0.00))

(defun scale-color (c q)
  (mapcar (lambda (x) (* x q)) c))

(defun blend-color (c1 c2 q)
  (mapcar (lambda (x1 x2) (+ (* x1 q) (* x2 (- 1.0 q)))) c1 c2))

(defun save-color-float (c)
  (apply #'save-color (mapcar (lambda (x) (floor (* 255 x))) c)))

(defun save-land-pixel (c)
  (let ((a (+ 0.5 (* 0.5 (/ (cell-alt c) *max-land-alt*)))))
    (save-color-float
     (blend-color
      (scale-color *forest* a)
      (scale-color *desert* a)
      (/ (cell-food c) 10.0)))))

(defun water-gradient (n w)
  (- n (* w (floor n *max-water-alt*))))

(defun save-water-pixel (w)
  (save-color 0 (water-gradient 96 w) (water-gradient 128 w)))

(defun save-cell-pixel (c)
  (cond ((is-occupied c) (save-bug-pixel (cell-bug c)))
	((is-water c) (save-water-pixel (cell-alt c)))
	((is-land c) (save-land-pixel c))
	(t (save-color 0 0 0))))

(defun convert-cmd ()
  (format nil "convert pic.pnm -adaptive-resize ~Ax~A pic-~5,'0d.gif"
	  (* 4 (map-width)) (* 4 (map-height)) *epoch*))

(defun convert-to-gif ()
  (uiop:run-program (convert-cmd) :output nil))

(defun save-picture (world)
  (unless *max-water-alt* (fill-alt world))
  (with-open-file (*out* (file-name) :direction :output :if-exists :supersede)
    (format *out* "P3~%~A ~A 255~%" (map-width) (map-height))
    (for-each-cell world (lambda (c) (save-cell-pixel c))))
  (convert-to-gif))
