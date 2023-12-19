;;; ppl.lisp -- probabilistic programming library

(mgl-pax:define-package :permo-ppl
  (:use #:permo-lisp #:permo-base #:mgl-pax)
  (:nicknames :ppl))
(in-package :ppl)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)

(defsection @ppl (:title "Probabilistic Programming")
  """A probabilistic program is a Lisp function that draws values from probability distributions.

  Drawing from probability distributions is different than simply calling a random number generator:

  - The joint likelihood of all the drawn values is accumulated. (See *LOGPROB*.)
  - Draws can be named and recorded for future refernece. (See *TRACE*.)
  - Draws can be pinned to specific values instead of randomly sampled. (See *PINNED*.)

  Running a probabilistic program yields the random choices that were made and the joint likelihood
  of those choices. The program can be re-run with tweaked values for its choices (as pinned values)
  to support Monte Carlo methods. Real-world observed data can also be incorporated as pinned values.

  This example program draws a random `center` from a wide Gaussian distribution and then draws three
  more values distributed tightly around that center:

  ```cl-transcript (:dynenv pax-dynenv)
  (with-probability ()
    (let ((center (gaussian 0d0 100d0 :id :center)))
      (gaussian center 1d0 :id :a)
      (gaussian center 1d0 :id :b)
      (gaussian center 1d0 :id :c)
      (pretty-print-hash-table *trace*)
      *logprob*))
  .. (DICT
  ..   :CENTER -88.67786660143865d0
  ..   :A -87.8901603867952d0
  ..   :B -88.28281593832418d0
  ..   :C -88.55750137294814d0
  ..  ) 
  => -9.069629467678245d0
```

  This example is the same except that it pins the value of `c` to represent a real-world observation:

  ```cl-transcript (:dynenv pax-dynenv)
  (with-probability (:pinned (dict :c 500d0))
    (let ((center (gaussian 0d0 100d0 :id :center)))
      (gaussian center 1d0 :id :a)
      (gaussian center 1d0 :id :b)
      (gaussian center 1d0 :id :c)
      (pretty-print-hash-table *trace*)
      *logprob*))
  .. (DICT
  ..   :CENTER -88.67786660143865d0
  ..   :A -87.8901603867952d0
  ..   :B -88.28281593832418d0
  ..   :C 500.0d0
  ..  ) 
  => -173279.8776987841d0

```

  Note that the second example returns the pinned value for `c` but because this value is far from
  `center` the log-likelihood score is much lower i.e. this pinned (\"observed\") value does not
  fit well with the others.
  """
  (*logprob*        variable)
  (*trace*          variable)
  (*pinned*         variable)
  (with-probability macro)
  (@ppl-random      section))

;;; Internal mechanisms

(defvar *logprob* nil
  "Accumulator for the log-probability of the current execution.")

(defvar *trace* nil
  "Hashtable recording the values that are drawn.")

(defvar *pinned* nil
  "Hashtable specifying any pinned values that must be observed instead of making random draws.")

(declaim (type (or null double-float) *logprob*)
         (type (or null hash-table) *trace* *pinned*))

(defmacro with-probability (options &body body)
  `(call-with-prob (lambda () ,@body) ,@options))

(defun call-with-prob (thunk &key pinned)
  "Call THUNK with *LOGPROB* and *TRACE* updates.
  PINNED is bound to *PINNED* if supplied."
  (let ((*logprob* 0d0)
        (*trace* (dict))
        (*pinned* pinned))
    (funcall thunk)))

(defun add-logprob (score)
  "Add SCORE to log probability accumulator (if enabled.)"
  (when *logprob*
    (incf *logprob* score)))

;;; Probability distributions

(defsection @ppl-random (:title "Random distributions")
  "Functions for drawing from random distributions accept these common optional arguments:
  - \X is the pinned value to observe instead of random sampling.
  - \ID is the key to record the drawn value in *TRACE*."
  (gaussian function)
  (uniform function)
  (categorical function)
  (bernoulli function)
  (exponential function)
  (laplace function)
  (uniform-discrete function))

(defun gaussian (μ σ &key x id)
  "Draw a Gaussian random value with mean μ and stddev σ."
  (lret ((value (or x (pinned-value id) (dist:gaussian-sample μ σ))))
    (add-logprob (dist:gaussian-log-likelihood μ σ value))
    (record id value)))

(defun uniform (min max &key x id)
  "Draw a uniform random value between MIN and MAX."
  (lret ((value (or x (pinned-value id) (dist:uniform-sample min max))))
    (add-logprob (dist:uniform-log-likelihood min max value))
    (record id value)))

(defun record (id value)
  (when (and *trace* id)
    (setf (@ *trace* id) value)))

(defun pinned? (id)
  (nth-value 1 (gethash id *pinned*)))

(defun pinned-value (id)
  (when *pinned* (values (gethash *pinned* id))))

(-> categorical ((soft-alist-of t r) &key (:x (or null r)) (:id t)) t)
(defun categorical (category-weight-alist &key x id)
  "Draw a _categorical_ random value from CATEGORY-WEIGHT-ALIST.
  The weights are unnormalized double-floats and the categories can be
  any Lisp value."
  (lret ((value (or x (pinned-value id) (dist:categorical-sample category-weight-alist))))
    (add-logprob (dist:categorical-log-likelihood category-weight-alist value))
    (record id value)))

(-> bernoulli (r &key (:x (member t nil)) (:id t)) (member t nil))
(defun bernoulli (p &key x id)
  (lret ((value (or x (pinned-value id) (dist:bernoulli-sample p))))
    (add-logprob (dist:bernoulli-log-likelihood p value))
    (record id value)))

(-> exponential (r &key (:x r>0) (:id t)) (r>=0))
(defun exponential (rate &key x id)
  (lret ((value (or x (pinned-value id) (dist:exponential-sample rate))))
    (add-logprob (dist:exponential-log-likelihood rate value))
    (record id value)))

(-> laplace (r r>0 &key (:x r>=0) (:id t)) r)
(defun laplace (location scale &key x id)
  (lret ((value (or x (pinned-value id) (dist:laplace-sample location scale))))
    (add-logprob (dist:laplace-log-likelihood location scale value))
    (record id value)))

(-> uniform-discrete (n>0 &key (:x n>=0) (:id t)) n>=0)
(defun uniform-discrete (n &key x id)
  (lret ((value (or x (pinned-value id) (dist:uniform-discrete-sample n))))
    (add-logprob (dist:uniform-discrete-log-likelihood n value))
    (record id value)))
  
