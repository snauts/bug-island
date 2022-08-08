#!/bin/bash

rm pic-*.pnm pic-*.gif -f

STEP=${STEP:-1}
SAVE=${SAVE:-nil}
DELAY=${DELAY:-'0.02'}
MAP=${MAP:-'map.lisp'}
REGROWTH=${REGROWTH:-'6'}
LIFESPAN=${LIFESPAN:-'14'}
MIGRATION=${MIGRATION:-'4'}
sbcl --noinform --load main.lisp \
     --eval "(setf *file* \"$MAP\")" \
     --eval "(setf *delay* $DELAY)" \
     --eval "(setf *step* $STEP)" \
     --eval "(setf *regrowth* $REGROWTH)" \
     --eval "(setf *lifespan* $LIFESPAN)" \
     --eval "(setf *low-food* $MIGRATION)" \
     --eval "(setf *save-picture* $SAVE)" \
     --eval "(top-level)" 2> debug.txt

if [ -f pic-00001.pnm ]; then
    for F in $(ls *.pnm)
    do
	convert $F -adaptive-resize 400% $F.gif
    done
fi

if [ -f pic-00001.pnm.gif ]; then
    gifsicle --colors 256 --loopcount -d 4 pic*.gif > final.gif
fi
