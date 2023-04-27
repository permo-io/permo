(defpackage #:smc
  (:use #:permo #:permo-lisp))
(in-package #:smc)

;;;; Types

;; Short-hand types.
;; Leaky abstractions: no secret what the concrete representations are.
(deftype permo:R   (&rest args) "Real number."            `(double-float ,@args))
(deftype permo:R*  ()           "Real vector."            '(simple-array R  (*)))
(deftype permo:P   ()           "Probability."            '(R 0d0 1d0))
(deftype permo:L   ()           "Log-likelihood."         'R)
(deftype permo:pdf ()           "Prob. density function." '(function (&rest R) L))

(def ⊥ most-negative-double-float
  "Log-probability of an impossible event. (exp ⊥) => 0d0.")

(declaim (inline smc smc/likelihood-tempering))

(-> R (number) R)
(defsubst permo:R (x) (coerce x 'R))

(-> R* (&rest R) R*)
(defsubst permo:R* (&rest xs) (coerce xs 'R*))

(-> gaussian-log-likelihood (r r r) r)
(defsubst permo:gaussian-log-likelihood (μ σ x)
  "Return the likelihood of Normal(μ,σ) at X."
  (if (plusp σ)
      (let ((z (/ (- x μ) σ)))
        (- 0d0
           (log σ)
           (/ (+ (* z z) (log (* pi 2))) 2)))
      most-negative-double-float))


;;;; DEFMODEL: Model definition DSL (WIP)

(defmacro permo:defmodel (name arglist &body log-likelihood)
  (destructuring-bind (args paramspecs) (split-sequence '&param arglist)
    #+nil (when (null args)       (error "DEFMODEL requires at least one argument."))
    (when (null paramspecs) (error "DEFMODEL requires at least one parameter."))
    (loop for (name min max) in paramspecs
          unless (and (symbolp name) (typep min 'double-float) (typep max 'double-float))
            do (error "Invalid parameter: ~a" (list name min max)))
    (flet ((name (suffix name)
             (make-symbol (concatenate 'string (string name) "." suffix))))
      (let* ((doc (if (stringp (first log-likelihood))
                      (pop log-likelihood)
                      ()))
             #+nil (weights (make-symbol "WEIGHTS"))
             (params         (mapcar #'first paramspecs))
             (ranges         (mapcar #'rest paramspecs))
             (vector.names   (mapcar (compose #'make-symbol #'symbol-name) params))
             (proposal.names (mapcar (curry #'name "P") params))
             (param.scales   (loop for (start end) in ranges
                                   ;; About three standard deviations between
                                   ;; particles on each parameter dimension.
                                   ;; Give them a chance to jitter to neighbors.
                                   collect (/ (abs (- end start)) (length params) 3))))
        `(defun ,name (observations &key (n-particles 100) (steps 100) (jitter-scales '(0.01d0 0.35d0 1d0)))
           ,@(ensure-list doc)
           (let (,@(loop for name in vector.names
                         for (min max) in ranges
                         collect `(,name (init-parameter-vector n-particles ,min ,max))))
             (labels ((log-likelihood (,@params ,@args)
                        (declare (type R ,@params ,@args))
                        ,@log-likelihood)
                      (particle-log-likelihood (i ,@args)
                        (declare (type R ,@args))
                        (log-likelihood ,@(loop for vector in vector.names
                                                collect `(aref ,vector i))
                                        ,@args))
                      (respawn! (parents)
                        (reorder! parents ,@vector.names))
                      (jitter! (metropolis-accept?)
                        (loop for jitter in jitter-scales do
                          (loop for i below n-particles
                                ,@(loop for param in params
                                        for vector in vector.names
                                        append `(for ,param = (aref ,vector i)))
                                for ll.old = (partial #'log-likelihood ,@params)
                                ,@(loop for proposal.name in proposal.names
                                        for param in params
                                        for scale in param.scales
                                        append `(for ,proposal.name = (+ ,param (* (gaussian-random) jitter (/ ,scale (expt n-particles ,(/ 1 (length params))))))))
                                for ll.new = (partial #'log-likelihood ,@proposal.names)
                                when (funcall metropolis-accept? ll.old ll.new)
                                  do (setf ,@(loop for vector in vector.names
                                                   for proposal in proposal.names
                                                   append `((aref ,vector i) ,proposal)))))))
               (values
                (smc/likelihood-tempering n-particles observations
                                          :log-likelihood #'particle-log-likelihood
                                          :respawn! #'respawn!
                                          :jitter! #'jitter!
                                          :temp-step (/ 1d0 steps))
                (dict ,@(loop for param in params
                              for vector in vector.names
                              collect (list 'quote param)
                              collect vector)))
                                         )))))))

(defun init-parameter-vector (length min max)
  "Initialize 'prior' values using uniform random sampling."
  ;; XXX Implement low-discrepancy sequences for Quasi-Monte Carlo.
  (loop with vec = (make-array (list length) :element-type 'R)
        for i below length
        do (setf (aref vec i)
                 (+ min (random (- max min))))
        finally (return vec)))

(defun reorder! (indices &rest arrays)
  "Atomically overwrite ARRAY[a][i] with ARRAY[a][indices[i]]."
  (loop for a in arrays
        for original = (copy-array a)
        do (loop for i from 0
                 for p in indices
                 do (setf (aref a i) (aref original p)))))

(defmodel line (x y &param
                  (m -10d0 10d0)
                  (c -10d0 10d0)
                  (σ #.double-float-epsilon 5d0))
  "Linear relationship between X and Y with Gaussian noise of constant scale:
     y ~ m*x + c + N(0,σ)
   Infers parameters M (gradient), C (intercept), and σ (standard deviation.)"
  (gaussian-log-likelihood (+ c (* m x)) σ y))


(-> line/pdf (R R R) pdf)
(defun permo:line/pdf (m c σ)
  (lambda (x y)
    (gaussian-log-likelihood (+ c (* m x)) σ y)))

(defmodel line/linear-noise (x y &param
                               (m -10d0 100d0)
                               (c -10d0 10d0)
                               (σ #.double-float-epsilon 10d0))
  (gaussian-log-likelihood (+ c (* m x))
                           (* x σ) y))

(-> line/linear-noise/pdf (R R R) pdf)
(defun line/linear-noise/pdf (m c σ)
  (lambda (x y)
    (gaussian-log-likelihood (+ c (* m x)) (* x σ) y)))

(-> uniform-mixture/pdf (&rest pdf) pdf)
(defun permo:uniform-mixture/pdf (&rest pdfs)
  (let ((log-normalizer (log (R (length pdfs)))))
    (lambda (&rest values)
      (loop for pdf in pdfs
            collecting (apply pdf values) into acc
            finally (return (log/ (logsumexp (coerce acc 'R*))
                                  log-normalizer))))))

#+nil
(defun discretize-1d (pdf min max steps)
  (lret ((array (make-array (list steps) :element-type 'L)))
    (loop for step below steps
          for x = (lerp (/ step (1- steps)) min max)
          do (setf (aref array step)
                   (funcall pdf x)))))

(defun discretize-2d (pdf x.min x.max x.steps y.min y.max y.steps)
  ;; Table of three-element rows: X, Y, LOGPROB
  (lret ((array (make-array (list (* x.steps y.steps) 3))))
    (loop for row from 0 below (array-dimension array 0)
          for x = (lerp (/ (rem row x.steps)        (1- x.steps)) x.min x.max)
          for y = (lerp (/ (truncate row x.steps) (1- x.steps)) y.min y.max)
          do (setf (aref array row 0) x
                   (aref array row 1) y
                   (aref array row 2) (funcall pdf x y)))))

(defmodel gaussian (x &param
                      (μ -10d0 10d0)
                      (σ #.double-float-epsilon 10d0))
  "Gaussian distribution:
     x ~ N(μ,σ)
   Infers mean and standard deviation."
  (gaussian-log-likelihood μ σ x))

(defmodel pi-circle (&param (x -0.5d0 0.5d0) (y -0.5d0 0.5d0))
  "Marginal-likelihood should be pi.
   The log-marginal-likelihood should therefore be log pi (~1.145)."
  (if (and (<= -0.5d0 x 0.5d0)
           (<= -0.5d0 y 0.5d0)
           (< (sqrt (+ (* x x) (* y y))) 0.5d0))
      (log 4d0)
      ⊥))


;;;; Sequential Monte Carlo (Particle Filter) sampler

(defsubst smc/likelihood-tempering (n-particles observations
                                    &key
                                      log-likelihood respawn! jitter!
                                      (temp-step 0.01d0))
  "Run a callback-driven Sequential Monte Carlo simulation using likelihood tempering.

   Callbacks:
   (PARTICLE-LOG-LIKELIHOOD particle datum) ⇒ log-likelihood
     Return log-likelihood of DATUM given the parameters of PARTICLE.
   (RESPAWN! parent-indices)
     Replace particles by overwriting them with the resampled PARENT-INDICES.
   (JITTER! metropolis-accept?)
     Rejuvenate particles by proposing moves to the function
       (METROPOLIS-ACCEPT? OLD-LOG-LIKELIHOOD-FN NEW-LOG-LIKELIHOOD-FN)
     which compares the likelihood ratios and returns true if the move is accepted."
  (local
    (def temp 0d0)
    (def prev-temp temp)
    (def log-weights        (make-array (list n-particles) :element-type 'R :initial-element 0d0))
    (def normalized-weights (make-array (list n-particles) :element-type 'R :initial-element 1d0))

    (-> tempered-log-likelihood (t) R)
    (defun tempered-log-likelihood (log-likelihood-fn &optional (temp temp))
      "Return the log-likelihood tempered with the current temperature."
      (declare (type (function (R R) R) log-likelihood-fn))
      (handler-case
          (loop for o in (or observations (list '()))
                summing (apply log-likelihood-fn o) into ll of-type R
                finally (return (logexpt ll temp)))
        (floating-point-overflow () sb-ext:double-float-negative-infinity)))
    (-> log-mean-likelihood () R)
    (defun log-mean-likelihood ()
      "Return the log mean likelihood of all particles."
      (log/ (logsumexp log-weights) (coerce (log n-particles) 'R)))
    (defun weight! ()
      "Calculate and set the weight of each particle."
      (loop for i below n-particles
            do (setf (aref log-weights i)
                     (log/ (tempered-log-likelihood (partial log-likelihood i))
                           (tempered-log-likelihood (partial log-likelihood i) prev-temp))))
      (loop with normalizer = (logsumexp log-weights)
            for i below n-particles
            for lw across log-weights
            do (setf (aref normalized-weights i) (exp (- lw normalizer)))))
    (defun resample? ()
      (adaptive-resample/ess? normalized-weights))
    (defun resample! ()
      "Replace old particles by resampling them into new ones."
      (funcall respawn! (systematic-resample normalized-weights)))
    (defun step! ()
      "Advance the tempering schedule (unless finished.)"
      (if (= temp 1d0)
          nil
          (progn (setf prev-temp temp)
                 (setf temp (min 1d0 (+ temp temp-step))))))
    (defun metropolis-accept? (old-log-likelihood new-log-likelihood)
      (< (log (random 1d0)) (log/ (tempered-log-likelihood new-log-likelihood)
                                  (tempered-log-likelihood old-log-likelihood))))
    (defun rejuvenate! ()
      (funcall jitter! #'metropolis-accept?))

    (smc :log-mean-likelihood #'log-mean-likelihood
         ;; XXX Have to resample ATM - have a bug in weight calculations when inhibited...
         :resample? (constantly t) #+nil #'resample?
         :resample! #'resample!
         :rejuvenate! #'rejuvenate!
         :step! #'step!
         :weight! #'weight!)))

(defsubst smc (&key log-mean-likelihood resample? resample! rejuvenate! step! weight!)
  "Run a callback-driven Sequential Monte Carlo particle filter simulation.
   Return the log marginal likelihood estimate.

   Callbacks:
   (WEIGHT!)
     Calculate and associate weights with particles.
   (LOG-MEAN-LIKELIHOOD) ⇒ double-float
     Return the log mean likelihood for all particles.
   (RESAMPLE!)
     Resample weighted particles into equally-weighted replacements.
   (REJUVENATE!)
     Jitter particles without changing their distribution.
   (STEP!) ⇒ boolean
     Advance the simulation. Return true on success, false on a completed simulation."
  (loop do (funcall weight!)
        sum (funcall log-mean-likelihood) of-type R
        while (funcall step!)
        do (when (funcall resample?)
             (funcall resample!))
           (funcall rejuvenate!)))

(defun adaptive-resample/ess? (normalized-weights &optional (threshold 0.7d0))
  "Does the Effective Sample Size justify resampling? (Adaptive Resampling.)"
  (< (effective-sample-size normalized-weights) (* (length normalized-weights) threshold)))

(-> effective-sample-size (R*) R)
(defun effective-sample-size (normalized-weights)
  "Return the Effective Sample Size."
  (/ 1 (loop for w across normalized-weights summing (expt w 2))))

;; Helpers for arithmetic in the log domain.
;; Used sparingly when it helps to clarify intent.
(-> log/ (r r) r)
(defun permo:log/ (a b) (- a b))
;;(defun log* (&rest numbers) (apply #'+ numbers))
(-> logexpt (r r) r)
(defun permo:logexpt (a b)
  "Raise A (a log-domain number) to the power B (which is not log!)"
  (* a b))

;;;; Systematic resampling

(-> systematic-resample (r*) list)
(defun systematic-resample (normalized-weights)
  "Return parent indices based on Systematic Resampling."
  (loop with n = (length normalized-weights)
        with cdf = (coerce (loop for weight across normalized-weights
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

;;;; Utilities

(-> logsumexp (R*) R)
(defun permo:logsumexp (vec)
  ;; utility for numerically stable addition of log quantities e.g. for normalization
  ;; see e.g. https://gregorygundersen.com/blog/2020/02/09/log-sum-exp/
  (let ((max (the R (reduce #'max vec))))
    (if (sb-ext:float-infinity-p max)
        sb-ext:double-float-negative-infinity
        (loop for x across vec
              summing (if (sb-ext:float-infinity-p x) 0d0 (exp (- x max))) into acc of-type R
              finally (return (the R (+ max (log acc))))))))
