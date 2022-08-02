(defstruct cell pos type food bug)

(defstruct bug pos age size)

(format t "Bug Island, inspired by Ellen Ullman's novel `the Bug`~%")

(load "map.lisp")

(defun pos (x y)
  (cons x y))

(defun pos-x (p)
  (car p))

(defun pos-y (p)
  (cdr p))

(defun map-size ()
  (pos (length (first *map*)) (length *map*)))
