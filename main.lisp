(defstruct cell pos type food bug)

(defstruct bug home age size)

(format t "Bug Island, inspired by Ellen Ullman's novel `the Bug`~%")

(load "math.lisp")
(load "map.lisp")

(defun map-size ()
  (list (length (elt *map* 0)) (length *map*)))

(defun make-map ()
  (make-array (map-size) :initial-element nil))

(defun iterate-map (map fn)
  (dotimes (y (array-dimension map 1))
    (dotimes (x (array-dimension map 0))
      (funcall fn x y))))

(defun for-each-cell (map fn)
  (iterate-map map (lambda (x y) (funcall fn (aref map x y)))))

(defun fill-pos (map fn)
  (lambda (x y) (setf (aref map x y) (funcall fn (make-pos x y)))))

(defun fill-map (map fn)
  (iterate-map map (fill-pos map fn)))

(defun char-from-map (p)
  (aref (aref *map* (pos-y p)) (pos-x p)))

(defparameter *max-food* 10)
(defparameter *low-food* 5)
(defparameter *no-food* 0)

(defun get-type (p)
  (case (char-from-map p)
    (#\space 'land)
    (#\. 'land)
    (#\* 'land)
    (#\- 'water)))

(defun get-food (p)
  (case (char-from-map p)
    (#\space *no-food*)
    (#\. *low-food*)
    (#\* *max-food*)))

(defun create-cell (p)
  (make-cell :pos p :bug nil :type (get-type p) :food (get-food p)))

(defun is-land (c)
  (eq 'land (cell-type c)))

(defun is-water (c)
  (eq 'water (cell-type c)))

(defun is-barren (c)
  (= (cell-food c) *no-food*))

(defun is-growable (c)
  (< *no-food* (cell-food c) *max-food*))

(defun is-rich (c)
  (> (cell-food c) *low-food*))

(defun land-char (c)
  (cond ((is-barren c) #\space)
	((is-rich c) #\*)
	(t #\.)))

(defun cell-char (c)
  (cond ((is-land c) (land-char c))
	((is-water c) #\-)
	(t #\?)))

(defun last-cell-in-a-row (c)
  (= (1- (first (map-size)))
     (pos-x (cell-pos c))))

(defun print-cell (c)
  (format t "~A" (cell-char c))
  (when (last-cell-in-a-row c)
    (format t "~%")))

(defun grow-cell (c)
  (when (and (is-land c) (is-growable c))
    (incf (cell-food c))))

(defun create-world ()
  (let ((world (make-map)))
    (fill-map world #'create-cell)
    world))

(defun bug-island (world)
  (let ((epoch 0))
    (loop
      (incf epoch)
      (format t "N=~A~%" epoch)
      (for-each-cell world #'grow-cell)
      (for-each-cell world #'print-cell)
      (sleep 1))))

(defun top-level ()
  (handler-case (bug-island (create-world))
    (condition (var) (format t "ERROR: ~A~%" var)))
  (uiop:quit 0))
