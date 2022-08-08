(defparameter *max-water-alt* nil)

(defun adjacent-water (world c)
  (let ((*fov* 1))
    (get-fov-cells world c)))

(defun test-height (i)
  (lambda (x) (and (numberp (cell-alt x)) (= (1- i) (cell-alt x)))))

(defun fill-alt (world)
  (setf *max-water-alt* 0)
  (dotimes (i (max (map-width) (map-height)))
    (for-each-cell
     world
     (lambda (c)
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

(defun save-land-pixel (f)
  (if (= f 0)
      (save-color 20 35 0)
      (save-color 0 (+ 55 (* 10 f)) 0)))

(defun water-gradient (n w)
  (- n (* w (floor n *max-water-alt*))))

(defun save-water-pixel (w)
  (save-color 0 (water-gradient 96 w) (water-gradient 128 w)))

(defun save-cell-pixel (c)
  (cond ((is-occupied c) (save-bug-pixel (cell-bug c)))
	((is-land c) (save-land-pixel (cell-food c)))
	((is-water c) (save-water-pixel (cell-alt c)))
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
