(defstruct cell pos type food bug fov)

(defstruct bug cell age size)

(defparameter *max-food* 10)
(defparameter *low-food* 5)

(defparameter *max-size* 10)
(defparameter *low-size* 5)
(defparameter *sow-size* 2)

(defparameter *old-age* 20)

(defparameter *fov* 5)

(format t "Bug Island, inspired by Ellen Ullman's novel `the Bug`~%")

(load "math.lisp")
;(load "map.lisp")
(load "map2.lisp")

(defun map-width ()
  (length (elt *map* 0)))

(defun map-height ()
  (length *map*))

(defun map-size ()
  (list (map-width) (map-height)))

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

(defun is-distance-one (c1 c2)
  (= 1 (distance (pos-sub (cell-pos c1) (cell-pos c2)))))

(defun adjacent-cells (c1)
  (delete-if-not (lambda (c2) (is-distance-one c1 c2)) (cell-fov c1)))

(defun has-nearby-forest (c1)
  (member-if (lambda (x) (<= *sow-size* (cell-food x)))
	     (adjacent-cells c1)))

(defun is-growable (c)
  (or (< 0 (cell-food c) *max-food*)
      (and (= 0 (cell-food c))
	   (has-nearby-forest c))))

(defun is-occupied (c)
  (not (null (cell-bug c))))

(defun is-rich (c)
  (> (cell-food c) *low-food*))

(defun is-forest (c)
  (= (cell-food c) *max-food*))

(defun land-char (c)
  (cond ((is-barren c) #\space)
	((is-forest c) #\*)
	(t #\.)))

(defun bug-char (b)
  (if (> (bug-size b) *low-size*) #\O #\o))

(defun cell-char (c)
  (cond ((is-occupied c) (bug-char (cell-bug c)))
	((is-land c) (land-char c))
	((is-water c) #\-)
	(t #\?)))

(defun last-cell-in-a-row (c)
  (= (1- (map-width))
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

(defun get-fov-pos (c)
  (mapcar (lambda (p) (pos-add (cell-pos c) p))
	  (circle *fov* (cell-pos c))))

(defun good-pos (p)
  (and (<= 0 (pos-x p) (1- (map-width)))
       (<= 0 (pos-y p) (1- (map-height)))))

(defun get-fov-cells (w c)
  (mapcar (lambda (p) (aref w (pos-x p) (pos-y p)))
	  (remove-if-not #'good-pos (get-fov-pos c))))

(defun visible-fov-cells (w c)
  (remove-if-not #'is-land (get-fov-cells w c)))

(defun step-to (p1 p2)
  (pos-add p1 (normalize (pos-sub p2 p1))))

(defun neighbor-of (c p)
  (first (member-if (lambda(c) (pos-eq p (cell-pos c))) (cell-fov c))))

(defun from-to (c1 c2)
  (neighbor-of c1 (step-to (cell-pos c1) (cell-pos c2))))

(defun is-walkable (c1 c2)
  (unless (or (null c1) (not (is-land c1)))
    (or (eq c1 c2) (is-walkable (from-to c1 c2) c2))))

(defun walkable-fov (c1)
  (lambda (c2) (is-walkable c1 c2)))

(defun walkable-fov-cells (w c)
  (declare (ignore w))
  (remove-if-not (walkable-fov c) (cell-fov c)))

(defun filter-fov-by (w fn)
  (for-each-cell w (lambda (c) (setf (cell-fov c) (funcall fn w c)))))

(defun generate-fov (w)
  (filter-fov-by w #'visible-fov-cells)
  (filter-fov-by w #'walkable-fov-cells))

(defun create-world ()
  (let ((world (make-map)))
    (fill-map world #'create-cell)
    (generate-fov world)
    (create-bugs world)
    world))

(defun collect-all-bugs (world)
  (let ((bugs nil))
    (for-each-cell
     world (lambda (c) (when (is-occupied c) (push (cell-bug c) bugs))))
    bugs))

(defun is-old (b)
  (>= (bug-age b) *old-age*))

(defun is-big (b)
  (>= (bug-size b) *max-size*))

(defun bug-dies (b)
  (setf (cell-bug (bug-cell b)) nil))

(defun bug-dead (b)
  (null (cell-bug (bug-cell b))))

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

(defun make-baby (c b)
  (setf (bug-size b) *low-size*)
  (add-bug c *low-size*))

(defun move-bug (b c1 c2)
  (when (not (is-occupied c2))
    (setf (bug-cell b) c2)
    (setf (cell-bug c2) b)
    (setf (cell-bug c1) nil)
    (when (is-big b)
      (make-baby c1 b))))

(defun best-move (b)
  (let ((fov (copy-list (cell-fov (bug-cell b)))))
    (first (sort (remove-if #'is-occupied fov) #'> :key #'cell-food))))

(defun is-greedy (src dst)
  (and (not (is-rich src))
       (or (= 0 (cell-food src))
	   (> (cell-food dst)
	      (cell-food src)))))

(defun bug-moves (b)
  (let ((dst (best-move b))
	(src (bug-cell b)))
    (when (and dst (or (is-big b) (is-greedy src dst)))
      (move-bug b src (from-to src dst)))))

(defun bug-lives (b)
  (unless (bug-eats b)
    (bug-starves b))
  (unless (bug-dead b)
    (bug-moves b)))

(defun bugs-life (b)
  (incf (bug-age b))
  (if (is-old b)
      (bug-dies b)
      (bug-lives b)))

(defun advance (world)
  (for-each-cell world #'grow-cell)
  (mapcar #'bugs-life (collect-all-bugs world)))

(defun roll-screen ()
  (dotimes (i (1+ (map-height)))
    (format t "~%")))

(defun bug-island (step world)
  (let ((epoch 0))
    (roll-screen)
    (loop
      (incf epoch)
      (let ((extinction (null (advance world))))
	(when (or extinction (= 0 (mod epoch step)))
	  (format t "~c[~AA" #\ESC (1+ (map-height)))
	  (format t "N=~A~%" epoch)
	  (for-each-cell world #'print-cell)
	  (if (not extinction)
	      (sleep (if (= 1 step) 0.05 1.0))
	      (quit)))))))

(defun top-level (&optional n)
  (handler-case (bug-island (or n 1) (create-world))
    (condition (var) (format t "ERROR: ~A~%" var)))
  (uiop:quit 0))
