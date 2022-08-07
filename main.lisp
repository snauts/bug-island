(defstruct cell pos type food bug fov)

(defstruct bug id cell age size prey)

(defparameter *max-food* 10)
(defparameter *low-food* 5)

(defparameter *max-size* 10)
(defparameter *low-size* 5)

(defparameter *regrowth* 2)
(defparameter *lifespan* 20)

(defparameter *fov* 5)

(defparameter *step* 1)
(defparameter *delay* 0)
(defparameter *file* "map.lisp")

(defvar *predator* t)
(defvar *identity* 0)
(defvar *epoch* 0)

(format t "Bug Island, inspired by Ellen Ullman's novel `the Bug`~%")

(load "math.lisp")
(load *file*)

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

(defun has-vegetation (c)
  (and (not (is-water c)) (not (is-barren c))))

(defun distance-less (len c1 c2)
  (>= len (distance (pos-sub (cell-pos c1) (cell-pos c2)))))

(defun adjacent-cells (c1 len)
  (remove-if-not (lambda (c2) (distance-less len c1 c2)) (cell-fov c1)))

(defun has-nearby-forest (c1)
  (member-if (lambda (x) (<= *regrowth* (cell-food x)))
	     (adjacent-cells c1 1)))

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

(defun color-code (n &optional (p ""))
  (format nil "~c[~Am~A" #\ESC n p))

(defun land-char (c)
  (cond ((is-forest c) (color-code 92 #\*))
	((is-barren c) #\space)
	(t (color-code 32 #\.))))

(defun is-predator (b)
  (numberp (bug-prey b)))

(defun turn-into-predator (b)
  (setf (bug-prey b) 0))

(defun is-grazer (b)
  (not (is-predator b)))

(defun bug-char (b)
  (let ((big (> (bug-size b) *low-size*))
	(bad (is-predator b)))
    (cond ((and (not big) (not bad))
	   (color-code 31 #\o))
	  ((and big (not bad))
	   (color-code 91 #\O))
	  ((and (not big) bad)
	   (color-code 35 #\x))
	  ((and big bad)
	   (color-code 95 #\X)))))

(defun cell-char (c)
  (cond ((is-occupied c) (bug-char (cell-bug c)))
	((is-land c) (land-char c))
	((is-water c) (color-code 36 #\-))
	(t #\?)))

(defun last-cell-in-a-row (c)
  (= (1- (map-width))
     (pos-x (cell-pos c))))

(defun print-cell (c)
  (format t "~A" (cell-char c))
  (when (last-cell-in-a-row c)
    (format t "~%")))

(defun being-grazed (c)
  (and (is-occupied c) (is-grazer (cell-bug c))))

(defun grow-cell (c)
  (when (and (is-land c)
	     (is-growable c)
	     (not (being-grazed c)))
    (incf (cell-food c))))

(defun id ()
  (incf *identity*))

(defun plant-bug (c b)
  (when b (setf (bug-cell b) c))
  (setf (cell-bug c) b))

(defun add-bug (c &key (size 1) (prey nil))
  (if (is-occupied c)
      (error "cell already occupied")
      (plant-bug c (make-bug :age 0 :size size :prey prey :id (id)))))

(defun dout (&rest rest)
  (apply #'format (cons *error-output* rest)))

(defun bug-pos (b)
  (cell-pos (bug-cell b)))

(defun debug-bug (b &optional (msg ""))
  (dout "BUG(~A): TIME=~A POS=(~A,~A) AGE=~A SIZE=~A PREY=~A"
	(bug-id b)
	*epoch*
	(pos-x (bug-pos b))
	(pos-y (bug-pos b))
	(bug-age b)
	(bug-size b)
	(bug-prey b))
  (when (> (length msg) 0)
    (dout " MESSAGE=~A" msg))
  (dout "~%"))

(defun create-bugs (world)
  (for-each-cell
   world
   (lambda (c)
     (when (is-land c)
       (return-from create-bugs (add-bug c))))))

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
  (load *file*)
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
  (>= (bug-age b) *lifespan*))

(defun is-big (b)
  (>= (bug-size b) *max-size*))

(defun bug-dies (b)
  (plant-bug (bug-cell b) nil)
  (setf (bug-cell b) nil))

(defun bug-dead (b)
  (null (bug-cell b)))

(defun bug-food (b &key (decrement 0))
  (if (is-predator b)
      (decf (bug-prey b) decrement)
      (decf (cell-food (bug-cell b)) decrement)))

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
  (add-bug c :size *low-size* :prey (bug-prey b))
  (when (is-predator b)
    (setf (bug-prey b) 0))) ; all the predator food stock goes to baby

(defun attack-prey (b c2)
  (when (and (is-occupied c2) (is-predator b))
    (let ((victim (cell-bug c2)))
      (when (is-grazer victim)
	(setf (bug-prey b) (bug-size victim))
	(bug-dies victim)))))

(defun move-bug (b c1 c2)
  (attack-prey b c2)
  (when (not (is-occupied c2))
    (plant-bug c2 b)
    (plant-bug c1 nil)
    (when (is-big b)
      (make-baby c1 b))))

(defun best-pasture (b)
  (let ((fov (copy-list (cell-fov (bug-cell b)))))
    (first (sort (remove-if #'is-occupied fov) #'> :key #'cell-food))))

(defun cell-meat (c)
  (let ((b (cell-bug c)))
    (and b (is-grazer b))))

(defun hunt-value (c)
  (bug-size (cell-bug c)))

(defun best-hunt (b)
  (let ((fov (copy-list (cell-fov (bug-cell b)))))
    (first (sort (remove-if-not #'cell-meat fov) #'> :key #'hunt-value))))

(defun best-move (b)
  (if (is-grazer b)
      (best-pasture b)
      (best-hunt b)))

(defun is-greedy (src dst)
  (and (not (is-rich src))
       (or (= 0 (cell-food src))
	   (> (cell-food dst)
	      (cell-food src)))))

(defun should-move (b src dst)
  (if (is-grazer b)
      (is-greedy src dst)
      (= 0 (bug-prey b))))

(defun bug-moves (b)
  (let ((dst (best-move b))
	(src (bug-cell b)))
    (when (and dst (or (is-big b) (should-move b src dst)))
      (move-bug b src (from-to src dst)))))

(defun occupied-count (b)
  (remove-if-not #'is-occupied (adjacent-cells (bug-cell b) 2)))

(defun surrouned (b)
  (= 8 (length (occupied-count b))))

(defun bug-lives (b)
  (when (and *predator* (surrouned b))
    (turn-into-predator b)
    (setf *predator* nil))
  (unless (bug-eats b)
    (bug-starves b))
  (unless (bug-dead b)
    (bug-moves b)))

(defun bugs-life (b)
  (unless (bug-dead b)
    (incf (bug-age b))
    (if (is-old b)
	(bug-dies b)
	(bug-lives b))))

(defun advance (world)
  (for-each-cell world #'grow-cell)
  (mapcar #'bugs-life (collect-all-bugs world)))

(defun roll-screen ()
  (dotimes (i (1+ (map-height)))
    (format t "~%")))

(defun print-simulation-statistics (world)
  (format t "~A" (color-code 39))
  (format t "N=~A " *epoch*)
  (let ((grazers 0)
	(predators 0)
	(vegetation 0))
    (for-each-cell
     world (lambda (c)
	     (when (has-vegetation c)
	       (incf vegetation))
	     (when (is-occupied c)
	       (if (is-predator (cell-bug c))
		   (incf predators)
		   (incf grazers)))))
    (format t "[GRAZERS=~A PREDATORS=~A VEGETATION=~A]~%"
	    grazers predators vegetation)))

(defun bug-island (world)
  (let ((*predator* t)
	(*identity* 0)
	(*epoch* 0))
    (roll-screen)
    (loop
      (incf *epoch*)
      (let ((extinction (null (advance world))))
	(when (or extinction (= 0 (mod *epoch* (max 1 *step*))))
	  (format t "~c[~AA" #\ESC (1+ (map-height)))
	  (print-simulation-statistics world)
	  (for-each-cell world #'print-cell)
	  (format t "~A" (color-code 39))
	  (if (not extinction)
	      (sleep *delay*)
	      (quit)))))))

(defun top-level ()
  (handler-case (bug-island (create-world))
    (condition (var) (format t "~AERROR: ~A~%" (color-code 39) var)))
  (uiop:quit 0))
