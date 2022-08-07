#!/bin/bash

STEP=${STEP:-1}
DELAY=${DELAY:-'0.02'}
MAP=${MAP:-'map.lisp'}
REGROWTH=${REGROWTH:-'2'}
LIFESPAN=${LIFESPAN:-'20'}
sbcl --noinform --load main.lisp \
     --eval "(setf *file* \"$MAP\")" \
     --eval "(setf *delay* $DELAY)" \
     --eval "(setf *step* $STEP)" \
     --eval "(setf *regrowth* $REGROWTH)" \
     --eval "(setf *lifespan* $LIFESPAN)" \
     --eval "(top-level)" 2> debug.txt
