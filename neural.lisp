; learning for me and learning for machine

(defstruct neuron
  weights bias net out delta) ; out = activate(net)

(defstruct network
  layers alpha activate derivative)

(defun sigmoid (x)
  (/ 1.0 (+ 1.0 (exp (- x)))))

(defun d-sigmoid (net out)
  (declare (ignore net))
  (* out (- 1.0 out))) ; optimize
  ; (* (sigmoid net) (- 1.0 (sigmoid net)))

(defun rnd-1 ()
  (- (random 2.0) 1.0))

(defun new-neuron (size)
  (let ((weights (make-array size :initial-element 0.0)))
    (dotimes (i (length weights)) (setf (aref weights i) (rnd-1)))
    (make-neuron :weights weights :bias (rnd-1) :net 0.0 :out 0.0 :delta 0.0)))

(defun activate (network neuron)
  (let ((a-fn (network-activate network)))
    (setf (neuron-out neuron) (funcall a-fn (neuron-net neuron)))))

(defun derivative (network neuron)
  (let ((d-fn (network-derivative network)))
    (funcall d-fn (neuron-net neuron) (neuron-out neuron))))

(defun fire-neuron (neuron previous-layer network)
  (setf (neuron-net neuron) (neuron-bias neuron))
  (let ((weights (neuron-weights neuron)))
    (dotimes (i (length weights))
      (let ((input (neuron-out (aref previous-layer i))))
	(incf (neuron-net neuron) (* input (aref weights i)))))
    (activate network neuron)))

(defun fire-layer (layer previous-layer network)
  (dotimes (i (length layer) layer)
    (fire-neuron (aref layer i) previous-layer network)))

(defun make-layers (sizes)
  (let ((all-layers nil) (inputs 0))
    (dolist (layer-size sizes (nreverse all-layers))
      (let ((layer (make-array layer-size)))
	(dotimes (index layer-size)
	  (setf (aref layer index) (new-neuron inputs)))
	(setf inputs layer-size)
	(push layer all-layers)))))

(defun set-inputs (network input)
  (let ((input-layer (first (network-layers network))))
    (dotimes (i (length input-layer))
      (setf (neuron-out (aref input-layer i))
	    (elt input i)))))

(defun fire-network (network &optional (layers (network-layers network)))
  (unless (null (rest layers))
    (fire-layer (second layers) (first layers) network)
    (fire-network network (rest layers))))

(defun forward-propagate (network input)
  (set-inputs network input)
  (fire-network network))

(defun output-layer (network)
  (first (last (network-layers network))))

(defun update-neuron-delta (network neuron d-error)
  (setf (neuron-delta neuron) (* d-error (derivative network neuron))))

(defun output-deltas (network target)
  (let ((output (output-layer network)))
    (dotimes (i (length output))
      (let ((neuron (aref output i)))
	(let ((d-error (- (elt target i) (neuron-out neuron))))
	  (update-neuron-delta network neuron d-error))))))

(defun hidden-deltas (network layer next-layer)
  (dotimes (i (length layer))
    (let ((neuron (aref layer i)) (d-error 0.0))
      (loop for next across next-layer do
	(incf d-error (* (aref (neuron-weights next) i) (neuron-delta next))))
      (update-neuron-delta network neuron d-error))))

(defun reverse-layers (network)
  (reverse (rest (network-layers network)))) ; strip input layer

(defun compute-hidden (network &optional (layers (reverse-layers network)))
  (unless (null (rest layers))
    (hidden-deltas network (second layers) (first layers))
    (compute-hidden network (rest layers))))

(defun compute-deltas (network target)
  (output-deltas network target)
  (compute-hidden network))

(defun update-layer (alpha layer input-layer)
  (loop for neuron across layer do
    (let ((weights (neuron-weights neuron))
	  (update (* alpha (neuron-delta neuron))))
      (incf (neuron-bias neuron) update)
      (dotimes (i (length weights))
	(let ((input (aref input-layer i)))
	  (incf (aref weights i) (* update (neuron-out input))))))))

(defun update-weights (network &optional (layers (network-layers network)))
  (unless (null (rest layers))
    (update-layer (network-alpha network) (second layers) (first layers))
    (update-weights network (rest layers))))

(defun back-propagate (network input target)
  (forward-propagate network input)
  (compute-deltas network target)
  (update-weights network))

(defun make-sigmoid (sizes &optional (alpha 0.5))
  (make-network :layers (make-layers sizes) :alpha alpha
		:activate #'sigmoid :derivative #'d-sigmoid))

(defun consult (network input)
  (forward-propagate network input)
  (map 'list #'neuron-out (output-layer network)))

(defun train (network data &optional (epoch 1))
  (dotimes (i epoch network)
    (dolist (datum data)
      (back-propagate network (first datum) (second datum)))))
