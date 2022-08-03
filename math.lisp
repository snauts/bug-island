(defun make-pos (x y)
  (cons x y))

(defun pos-x (p)
  (car p))

(defun pos-y (p)
  (cdr p))

(defun pos-add (a b)
  (make-pos
   (+ (pos-x a) (pos-x b))
   (+ (pos-y a) (pos-y b))))

(defun pos-sub (a b)
  (make-pos
   (- (pos-x a) (pos-x b))
   (- (pos-y a) (pos-y b))))

(defun pos-eq (a b)
  (and (= (pos-x a) (pos-x b))
       (= (pos-y a) (pos-y b))))

(defun normalize (p)
  (make-pos
   (signum (pos-x p))
   (signum (pos-y p))))

(defun distance (p)
  (+ (expt (pos-x p) 2)
     (expt (pos-y p) 2)))

(defun circle (l)
  (let ((result nil))
    (loop for y from (- l) to l do
      (loop for x from (- l) to l do
	(let* ((p (make-pos x y))
	       (d (distance p)))
	  (when (and (< 0 d (1+ (expt l 2))))
	    (push p result)))))
    result))
