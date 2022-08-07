#!/bin/bash

STEP=${STEP:-1}
DELAY=${DELAY:-'0.02'}
MAP=${MAP:-'map.lisp'}
sbcl --noinform --load main.lisp \
     --eval "(setf *file* \"$MAP\")" \
     --eval "(setf *delay* $DELAY)" \
     --eval "(setf *step* $STEP)" \
     --eval "(top-level)" 2> debug.txt
