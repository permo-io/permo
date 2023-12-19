(mgl-pax:define-package :smc2
  (:use #:permo-lisp #:permo-base #:mgl-pax))

(in-package :smc2)

;;; Introduction

;;; Utilities

(defun histogram (data n-buckets)
  "Return an N-BUCKETS histogram of DATA as a list.
  The buckets span the minimum to maximum value with constant width.

  ```cl-transcript (:dynenv pax-dynenv)
  (histogram (loop repeat 1000 collect (gaussian-random)) 10)
  => (4 38 123 222 291 204 91 19 6 2)
  ```"
  (multiple-value-bind (min max) (extrema data #'<)
    (loop with buckets = (make-array n-buckets)
          for x across (coerce data 'vector)
          for bucket = (floor (min (1- n-buckets)
                                   (* n-buckets (/ (- x min)
                                                   (- max min)))))
          do (incf (aref buckets bucket))
          finally (return (coerce buckets 'list)))))

;;; Log weights

(defsection @log-weights (:title "Log weights (unnormalized)")
  "
```cl-transcript (:dynenv pax-dynenv)
(make-log-weights (r* (loop repeat 10000 collect (gaussian-random))))
==> #<LOG-WEIGHTS 10000 [-3.5d+0▁▁▄█▆▃▁▁3.9d+0]>
```
")

(defstruct (log-weights (:constructor make-log-weights (vec)))
  (vec (required-argument) :type R*))

(defmethod print-object ((w log-weights) stream)
  (print-unreadable-object (w stream :type t)
    (with-slots (vec) w
      (format stream "~D [~,1E~A~,1E]"
              (length vec)
              (reduce #'min vec)
              (cl-spark:spark (histogram vec 8))
              (reduce #'max vec)))))

;;; Normalized weights

;;; Not log scale. Sum to one.
(defstruct (normalized-weights (:constructor make-normalized-weights (vec)))
  (vec (required-argument) :type R*))

(-> normalize! (log-weights normalized-weights) t)
(defun normalize! (log-weights normalized-weights)
  "Normalize LOG-WEIGHTS and store the results into NORMALIZED-WEIGHTS.
  ```cl-transcript (:dynenv pax-dynenv)
  (lret* ((lw (make-log-weights (r* (loop repeat #1=10000 collect (gaussian-random)))))
          (nw (make-normalized-weights (make-array #1# :element-type 'double-float))))
    (normalize! lw nw))
  ==> #<NORMALIZED-WEIGHTS N=10000 ESS=3602.29>
```"
  (vector-normalize! (log-weights-vec log-weights)
                     (normalized-weights-vec normalized-weights)))

(defun vector-normalize! (log-vec norm-vec)
  "Perform NORMALIZE! on vectors LOG-VEC and NORM-VEC."
  (loop with normalizer = (logsumexp log-vec)
        for i below (length log-vec)
        do (setf (aref norm-vec i)
                 (exp (- (aref log-vec i) normalizer)))))

(-> effective-sample-size (normalized-weights) r)
(defun effective-sample-size (normalized-weights)
  "Return the effective sample size (ESS) of NORMALIZED-WEIGHTS."
  (vector-effective-sample-size (slot-value normalized-weights 'vec)))

(-> vector-effective-sample-size (r*) r)
(defun vector-effective-sample-size (normalized-weights-vector)
  "Return the effective sample size (ESS) of NORMALIZED-WEIGHTS-VECTOR."
  (/ 1d0 (loop for w across normalized-weights-vector summing (expt w 2))))

(defmethod print-object ((w normalized-weights) stream)
  (print-unreadable-object (w stream :type t)
    (with-slots (vec) w
      (format stream "N=~D ESS=~,2F"
              (length vec)
              (effective-sample-size w)))))

;;; Resampling

(-> systematic-resample (normalized-weights) list)
(defun systematic-resample (normalized-weights)
  "Return parent indices based on Systematic Resampling.
  ```cl-transcript
  (let ((weights '(0.1d0 0.1d0 0.1d0 0.1d0 0.1d0 0.5d0)))
    (systematic-resample (make-normalized-weights (r* weights))))
  => (0 1 3 5 5 5)
  ```"
  (loop with vec = (normalized-weights-vec normalized-weights)
        with n = (length vec)
        with cdf = (coerce (loop for weight across vec
                                 sum weight into cumulative
                                 collect cumulative)
                           'R*)
        with index = 0
        repeat n
        ;; Pick a starting offset into the CDF
        for u of-type R = (random (/ 1d0 n)) then (+ u (/ 1d0 n))
        ;; Step forward through the CDF
        do (loop while (and (> u (aref cdf (min index (1- n))))) do (incf index))
           (minf index (1- n))
        collect (min index (1- n))))

;;; Particles

(defsection @smc2-manual (:title "SMC2 Manual")
  "Here is some text."
  #+nil (foo function)
  #+nil (var class)
  #+nil (var-min structure-accessor)
  #+nil (var-max structure-accessor))

(defvar *particles* )

(defstruct particles
  (table      (required-argument) :type R**)
  (parameters (required-argument) :type list))

(defmethod print-object ((ps particles) stream)
  (print-unreadable-object (ps stream :type t)
    (with-slots (table parameters) ps
      (format stream "~{~a~^ ~}"
              (loop for parameter in parameters
                    for d below (array-dimension table 0)
                    for row = (make-array (array-dimension table 1)
                                          :element-type 'double-float
                                          :displaced-to table
                                          :displaced-index-offset (* d (array-dimension table 1)))
                    for hist = (histogram row 8)
                    collect (particle-parameter-summary parameter row) stream)))))

(defun particle-parameter-summary (name values &optional (histogram-width 8))
  (receive (min max) (extrema values #'<)
    (format nil "~A [~,1E,~,1E] ~A"
            name min max (cl-spark:spark (histogram values histogram-width)))))

;;; Prior construction via weighted random sampling

#+nil
(defun plausible-prior! (particles)
  
  )


#+nil
(defun reservoir-sample (likelihood-function parameters n-samples
                         &key (n-proposals (* samples 100)) (transform #'exp))
  (loop with particles = (make-particles :table (make-array (list n-dimensions n-samples) :element-type 'R)
                                         :parameters parameters)
        with proposal = (make-array (length parameters) :element-type 'R)
        with range = (funcall transform most-positive-double-float)
        do (loop for i in (length proposal)
                 do (setf (aref proposal i)
                          (* (if (random 2) 1 -1)
                             (funcall transform (random range))
                          (funcall)
                          ()))
  )))

;;; Simulations

(defstruct (simulation (:conc-name #:s.))
  (model        (required-argument) :type model)
  (particles    (required-argument) :type particles)
  (weights      (required-argument) :type log-weights)
  (weights.norm (required-argument) :type normalized-weights))

#|
(-> step (simulation) t)
(defun step (sim)

  )
|#

;;; Models

(deftype model () t)

#|

(defun random-model ()
  (one-of `(+ (random-model) (random-model))))

const ::= <number>

var ::= X
var ::= (f (X))

val = const | var


exp ::= var
exp ::= (op var val)

var ::= (op var var)

f  ::= (expt base) | (log base)
op ::= + | - | * | /

(+ x :c e)
(+ x :c e)
(+ (* x :m) :c e)
(+ (* (f x) :m) :c e)


(+ (* (+ x ϵ) :m))

(+ (* + (transform x) (gaussian)
 (~ noise (gaussian)))
   (+ c (~ noise (gaussian))))


(defun linear-model ()
  `(+ (* x :m) :c))

(defun gaussian-model ()
  `()

(~> (copy-sequence 'vector data)
      (sort _ #'<)
      (batches (ceiling (length data) n))
      (map 'list #'median _)))

  (mapcar #'median
          (sort (copy-sequence 'vector data) #'<))
  (loop with ordered = (sort (copy-sequence 'vector numbers) #'<)
        repeat elements
        for i from 0 by (/ (length numbers) elements)
        collect (elt ordered (floor i))))

  (let* ((ordered (sort (copy-seq 'vector numbers) #'<))
         ))
  (append (first numbers))
  (loop for index from )
  (let* ((vec (sort (copy-sequence 'vector numbers) #'<))
         (buckets (batches vec (ceiling (/ (length vec) size))))
         )
  )

(defstruct tempering-sequence
  )

(defsubst p (particle parameter &optional (ps *particles*))
  "Return the parameter value for indices PARTICLE and PARAMETER."
  (aref ps i j))

(defstruct var
  min max)

(deftype → (type)
  `(simple-array ,type (*)))

(define-symbol-macro × (error "undefined value"))

(defstruct model
  (output     × :type (→ var))
  (inputs     × :type var)
  (likelihood × :type (function (&rest R) R)))

(defun foo (x)
  "Mathematical function of X.
   ```cl-transcript
   (foo 41)
   ```"
  (1+ x))
|#

;;; API

#+nil
(defun simulate (likelihood-fn parameters prior-particles &key prior-weigths)
  "Simulate tempering LIKELIHOOD-FN into PRIOR-PARTICLES.
  Return two values: posterior particles and log marginal likelihood estimate."
  (let ((*particles* (make-particles ))))
  )

;;(-> infer-models ((soft-list-of variable r**) (soft-list-of model)))
#+nil
(defun infer-models (variables data &optional n)
  (let* ((models (generate-models))
         (fits (mapcar #'posetior models (first-half data)))
         (evidence posterior (mapcar #'marginal-likelihood models (first-half data)))
         (result (systematic-sample evidence posterior)))
    result))
