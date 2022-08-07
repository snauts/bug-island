#!/bin/bash

STEP=${STEP:-1}
DELAY=${DELAY:-'0.02'}
FILE=${FILE:-'map.lisp'}
sbcl --noinform --load main.lisp \
     --eval "(setf *file* \"$FILE\")" \
     --eval "(setf *delay* $DELAY)" \
     --eval "(setf *step* $STEP)" \
     --eval "(top-level)" 2> debug.txt
