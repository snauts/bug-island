#!/bin/bash

STEP=${STEP:-1}
DELAY=${DELAY:-'0.02'}
MAP=${MAP:-'map.lisp'}
REGROWTH=${REGROWTH:-'6'}
LIFESPAN=${LIFESPAN:-'14'}
MIGRATION=${MIGRATION:-'5'}
sbcl --noinform --load main.lisp \
     --eval "(setf *file* \"$MAP\")" \
     --eval "(setf *delay* $DELAY)" \
     --eval "(setf *step* $STEP)" \
     --eval "(setf *regrowth* $REGROWTH)" \
     --eval "(setf *lifespan* $LIFESPAN)" \
     --eval "(setf *low-food* $MIGRATION)" \
     --eval "(top-level)" 2> debug.txt
