(defvar *out* nil)

(defun file-name ()
  (format nil "pic-~5,'0d.pnm" *epoch*))

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

(defun save-cell-pixel (c)
  (cond ((is-occupied c) (save-bug-pixel (cell-bug c)))
	((is-land c) (save-land-pixel (cell-food c)))
	((is-water c) (save-color 0 #x80 #xa0))
	(t (save-color 0 0 0))))

(defun save-picture (world)
  (with-open-file (*out* (file-name) :direction :output :if-exists :supersede)
    (format *out* "P3~%~A ~A 255~%" (map-width) (map-height))
    (for-each-cell world (lambda (c) (save-cell-pixel c)))))
