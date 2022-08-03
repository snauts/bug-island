(defstruct cell pos type food bug)

(defstruct bug pos age size)

(format t "Bug Island, inspired by Ellen Ullman's novel `the Bug`~%")

(load "math.lisp")
(load "map.lisp")

(defun map-size ()
  (make-pos :x (length (elt +map+ 0)) :y (length +map+)))
