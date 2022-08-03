(defstruct cell pos type food bug fov)

(defstruct bug cell age size)

(defparameter *max-food* 10)
(defparameter *low-food* 5)

(defparameter *max-size* 10)
(defparameter *low-size* 5)

(defparameter *old-age* 20)

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

(defun get-type (p)
  (case (char-from-map p)
    (#\space 'land)
    (#\. 'land)
    (#\* 'land)
    (#\- 'water)))

(defun get-food (p)
  (case (char-from-map p)
    (#\. *low-food*)
    (#\* *max-food*)
    (#\space 0)))

(defun create-cell (p)
  (make-cell :pos p :bug nil :type (get-type p) :food (get-food p)))

(defun is-land (c)
  (eq 'land (cell-type c)))

(defun is-water (c)
  (eq 'water (cell-type c)))

(defun is-barren (c)
  (= 0 (cell-food c)))

(defun is-growable (c)
  (< 0 (cell-food c) *max-food*))

(defun is-occupied (c)
  (not (null (cell-bug c))))

(defun is-rich (c)
  (> (cell-food c) *low-food*))

(defun land-char (c)
  (cond ((is-barren c) #\space)
	((is-rich c) #\*)
	(t #\.)))

(defun bug-char (b)
  (if (> (bug-size b) *low-size*) #\O #\o))

(defun cell-char (c)
  (cond ((is-occupied c) (bug-char (cell-bug c)))
	((is-land c) (land-char c))
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
  (when (and (is-land c)
	     (is-growable c)
	     (not (is-occupied c)))
    (incf (cell-food c))))

(defun add-bug (c size)
  (if (is-occupied c)
      (error "cell already occupied")
      (setf (cell-bug c) (make-bug :cell c :age 0 :size size))))

(defun create-bugs (world)
  (mapc (lambda (p) (add-bug (aref world (pos-x p) (pos-y p)) 1))
	*bugs*))

(defun create-world ()
  (let ((world (make-map)))
    (fill-map world #'create-cell)
    (create-bugs world)
    world))

(defun collect-all-bugs (world)
  (let ((bugs nil))
    (for-each-cell
     world
     (lambda (c) (when (is-occupied c)
		   (push (cell-bug c) bugs))))
    bugs))

(defun is-old (b)
  (>= (bug-age b) *old-age*))

(defun is-big (b)
  (>= (bug-size b) *max-size*))

(defun bug-dies (b)
  (setf (cell-bug (bug-cell b)) nil))

(defun bug-food (b &key (decrement 0))
  (decf (cell-food (bug-cell b)) decrement))

(defun bug-eats (b)
  (let* ((is-food (> (bug-food b) 0)))
    (when is-food
      (bug-food b :decrement 1)
      (when (not (is-big b))
	(incf (bug-size b))))
    is-food))

(defun bug-starves (b)
  (cond ((= 0 (bug-size b))
	 (bug-dies b))
	((= 0 (bug-food b))
	 (decf (bug-size b)))))

(defun bug-lives (b)
  (unless (bug-eats b)
    (bug-starves b)))

(defun bugs-life (b)
  (incf (bug-age b))
  (if (is-old b)
      (bug-dies b)
      (bug-lives b)))

(defun advance (world)
  (for-each-cell world #'grow-cell)
  (mapc #'bugs-life (collect-all-bugs world)))

(defun bug-island (world)
  (let ((epoch 0))
    (loop
      (incf epoch)
      (advance world)
      (format t "N=~A~%" epoch)
      (for-each-cell world #'print-cell)
      (sleep 0.2))))

(defun top-level ()
  (handler-case (bug-island (create-world))
    (condition (var) (format t "ERROR: ~A~%" var)))
  (uiop:quit 0))
