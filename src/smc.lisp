(defpackage #:permo (:use #:gt))
(in-package #:permo)

(declaim (optimize (speed 1) (debug 3)))

;;;; Types

;; Short-hand types.
;; Leaky abstractions: no secret what the concrete representations are.
(deftype R   (&rest args) "Real number."            `(double-float ,@args))
(deftype R*  ()           "Real vector."            '(simple-array R  (*)))
(deftype R** ()           "Vector of real vectors." '(simple-array R* (*)))
(deftype P   ()           "Probability."            '(R 0d0 1d0))
(deftype L   ()           "Log-likelihood."         '(R * #.(log 1)))


;;;; DEFMODEL: Model definition DSL (WIP)

(defmacro defmodel (name arglist &body log-likelihood)
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
             (proposal.names (mapcar (curry #'name "P") params)))
        `(defun ,name (&key n-particles observations tempering-steps (jitter-scales '(0.01d0 0.1d0 0.5d0)))
           ,@(ensure-list doc)
           (let (,@(loop for name in vector.names
                         for (min max) in ranges
                         collect `(,name (init-parameter-vector n-particles ,min ,max))))
             (labels ((log-likelihood (,@params ,@args)
                        ,@log-likelihood)
                      (particle-log-likelihood (i ,@args)
                        (log-likelihood ,@(loop for vector in vector.names
                                                collect `(aref ,vector i))
                                        ,@args))
                      (respawn! (parents)
                        (reorder! parents ,@vector.names))
                      (jitter! (metropolis-accept?)
                        (loop for stddev in jitter-scales do
                          (loop for i below n-particles
                                ,@(loop for param in params
                                        for vector in vector.names
                                        append `(for ,param = (aref ,vector i)))
                                for ll.old = (partial #'log-likelihood ,@params)
                                ,@(loop for proposal.name in proposal.names
                                        for param in params
                                        append `(for ,proposal.name = (add-noise ,param stddev)))
                                for ll.new = (partial #'log-likelihood ,@proposal.names)
                                when (funcall metropolis-accept? ll.old ll.new)
                                  do (setf ,@(loop for vector in vector.names
                                                   for proposal in proposal.names
                                                   append `((aref ,vector i) ,proposal))))))
                      (add-noise (x stddev)
                        (+ x (* stddev (gaussian-random)))))
               #+debug (declare (inline (smc/likelihood-tempering)))
               (values
                (smc/likelihood-tempering n-particles observations
                                          :log-likelihood #'particle-log-likelihood
                                          :respawn! #'respawn!
                                          :jitter! #'jitter!
                                          :temp-step (/ 1d0 tempering-steps))
                (dict ,@(loop for param in params
                              for vector in vector.names
                              collect (list 'quote param)
                              collect vector)))
                                         )))))))

(defun init-parameter-vector (length min max)
  (loop with vec = (make-array (list length) :element-type 'R)
        for i below length
        do (setf (aref vec i) (lerp (/ i length) min max))
        finally (return vec)))

(defun reorder! (indices &rest arrays)
  "Atomically overwrite ARRAY[a][i] with ARRAY[a][indices[i]]."
  (loop for a in arrays
        for original = (copy-array a)
        do (loop for i from 0
                 for p in indices
                 do (setf (aref a i) (aref original p)))))

(defmodel pi-circle (&param (x -1d0 1d0) (y -1d0 1d0))
  "Estimate Pi via marginal likelihood."
  ;; XXX: This model starts with a reasonable estimate which then approaches
  ;;      zero as the number of likelihood tempering steps increases.
  ;;      Why?
  (log (if (< (sqrt (+ (* x x) (* y y))) 1d0)
           4d0
           0.001d0)))

(defmodel line (x y &param
                  (m -10d0 10d0)
                  (c -10d0 10d0)
                  (σ 0.001d0 5d0))
  "Linear relationship between X and Y with Gaussian noise of constant scale:
    y = m*x + c + N(0,σ)
   Infers parameters M (gradient), C (intercept), and σ (standard deviation.)"
  (gaussian-log-likelihood (+ c (* m x)) σ y))


;;;; Models written by hand

(defun smc/gaussian (&optional
                       (n-particles 1000)
                       (observations (loop repeat 1000 collect (+ 1d0 (* 2d0 (gaussian-random))))
                                     #+nil (loop repeat 1000 collect 100d0)))
  (local
    ;; Particles are represented as indices into these arrays.
    (def μ (make-array (list n-particles) :element-type 'double-float))
    (def σ (make-array (list n-particles) :element-type 'double-float))

    (loop for i below n-particles
          do (setf (aref μ i) (- 10d0 (random 20d0))
                   (aref σ i) (random 10d0)))

    (-> log-likelihood (double-float double-float double-float) double-float)
    (defun log-likelihood (μ σ datum)
      "Return the log-likelihood of DATUM for given model parameters."
      (values (if (plusp σ)
                  (gaussian-log-likelihood μ σ datum)
                  sb-ext:double-float-negative-infinity)))

    (defun particle-log-likelihood (particle datum)
      "Return the log-likelihood of DATUM given the parameters of PARTICLE."
      (log-likelihood (aref μ particle) (aref σ particle) datum))

    (defun respawn! (parent-indices)
      "Re-initialize the particles by copying values from PARENT-INDICES.
       (PARENT-INDICES were chosen by resampling.)"
      (loop with μ₀ = (copy-array μ)
            with σ₀ = (copy-array σ)
            for i from 0
            for p in parent-indices
            do (setf (aref μ i) (aref μ₀ p)
                     (aref σ i) (aref σ₀ p))))

    (defun jitter! (metropolis-accept?)
      "Jitter (rejuvenate) the parameters of all particles using one or more metropolis steps.

       Function (METROPOLIS-ACCEPT? OLD PROPOSED) uses the supplied log-likelihood functions
       to accept or reject the proposed state."
      ;; Try a series of moves of different relative sizes.
      (loop for proposal-stddev in '(0.01d0 0.10d0 0.25d0) do
        ;; Add gaussian noise to both parameters. Scaled to a fraction of current value.
        (loop for i below n-particles
              for μ.i = (aref μ i)
              for σ.i = (aref σ i)
              for μ.p = (+ μ.i (* μ.i proposal-stddev (gaussian-random)))
              for σ.p = (+ σ.i (* σ.i proposal-stddev (gaussian-random)))
              for old-log-likelihood = (partial #'log-likelihood μ.i σ.i)
              for new-log-likelihood = (partial #'log-likelihood μ.p σ.p)
              when (funcall metropolis-accept? old-log-likelihood new-log-likelihood)
                do (setf (aref μ i) μ.p
                         (aref σ i) σ.p))))
    (values μ σ (smc/likelihood-tempering n-particles observations
                                          :log-likelihood #'particle-log-likelihood
                                          :respawn! #'respawn!
                                          :jitter! #'jitter!))))

(defun smc/line (&optional
                   (n-particles 1000)
                   (observations (loop for i from 1 below 1000
                                       collect (list i i))))
  (local
    ;; y = m*x + c + N(0,σ)
    (def m (make-array (list n-particles))) ; m = gradient
    (def c (make-array (list n-particles))) ; c = intercept
    (def σ (make-array (list n-particles))) ; σ = stddev of ϵ i.e. gaussian noise

    (loop for i below n-particles
          do (setf (aref m i) (gaussian-random)
                   (aref c i) (gaussian-random)
                   (aref σ i) (abs (gaussian-random))))

    (-> log-likelihood (double-float double-float double-float list) double-float)
    (defun log-likelihood (m c σ datum)
      (destructuring-bind (x y) datum
        (values (if (plusp σ)
                    (gaussian-log-likelihood (+ (* m x) c) σ y)
                    sb-ext:double-float-negative-infinity))))
    (defun particle-log-likelihood (i datum)
      (log-likelihood (aref m i) (aref c i) (aref σ i) datum))
    (defun respawn! (parent-indices)
      (loop with m₀ = (copy-array m)
            with c₀ = (copy-array c)
            with σ₀ = (copy-array σ)
            for i from 0
            for p in parent-indices
            do (setf (aref m i) (aref m₀ p)
                     (aref c i) (aref c₀ p)
                     (aref σ i) (aref σ₀ p))))
    (defun jitter! (metropolis-accept?)
      (loop for proposal-stddev in '(0.01d0 0.10d0 0.25d0) do
        (loop for i below n-particles
              for m.i = (aref m i)
              for c.i = (aref c i)
              for σ.i = (aref σ i)
              for m.p = (+ m.i (* proposal-stddev (gaussian-random)))
              for c.p = (+ c.i (* proposal-stddev (gaussian-random)))
              for σ.p = (+ σ.i (* proposal-stddev (gaussian-random)))
              for old-log-likelihood = (partial #'log-likelihood m.i c.i σ.i)
              for new-log-likelihood = (partial #'log-likelihood m.p c.p σ.p)
              when (funcall metropolis-accept? old-log-likelihood new-log-likelihood)
                do (setf (aref m i) m.p
                         (aref c i) c.p
                         (aref σ i) σ.p))))
    (values m c σ (smc/likelihood-tempering n-particles observations
                                            :log-likelihood #'particle-log-likelihood
                                            :respawn! #'respawn!
                                            :jitter! #'jitter!))))


;;;; Sequential Monte Carlo (Particle Filter) sampler

(defun smc (&key log-mean-likelihood resample! rejuvenate! step! weight!)
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
        sum (funcall log-mean-likelihood)
        while (funcall step!)
        do (funcall resample!)
           (funcall rejuvenate!)))

(defun smc/likelihood-tempering (n-particles observations
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
    (def log-weights (make-array (list n-particles) :element-type 'double-float))

    (defun tempered-log-likelihood (log-likelihood-fn &optional (temp temp))
      "Return the log-likelihood tempered with the current temperature."
      (handler-case
          (loop for o in (or observations (list '()))
                summing (apply log-likelihood-fn o) into ll
                finally (return (logexpt ll temp)))
        (floating-point-overflow () sb-ext:double-float-negative-infinity)))
    (defun log-mean-likelihood ()
      "Return the log mean likelihood of all particles."
      (log/ (logsumexp log-weights) (log n-particles)))
    (defun weight! ()
      "Calculate and set the weight of each particle."
      (loop for i below n-particles
            do (let (#+nil (old (aref log-weights i)))
                 (setf (aref log-weights i)
                       (lret ((r (log/ (tempered-log-likelihood (partial log-likelihood i))
                                       (tempered-log-likelihood (partial log-likelihood i) prev-temp)))
                             #+nil (format t "~&r = ~8f~%" r)))))))
    (defun resample! ()
      "Replace old particles by resampling them into new ones."
      (funcall respawn! (systematic-resample log-weights)))
    (defun step! ()
      "Advance the tempering schedule (unless finished.)"
      (setf prev-temp temp)
      (and (< temp 1d0)
           (setf temp (min 1d0 (+ temp temp-step)))))
    (defun metropolis-accept? (old-log-likelihood new-log-likelihood)
      (< (log (random 1d0)) (log/ (tempered-log-likelihood new-log-likelihood)
                                  (tempered-log-likelihood old-log-likelihood))))
    (defun rejuvenate! ()
      (funcall jitter! #'metropolis-accept?))

    (smc :log-mean-likelihood #'log-mean-likelihood
         :resample! #'resample!
         :rejuvenate! #'rejuvenate!
         :step! #'step!
         :weight! #'weight!)))

;; Helpers for arithmetic in the log domain.
;; Used sparingly when it helps to clarify intent.
(defun log/ (&rest numbers) (apply #'- numbers))
;;(defun log* (&rest numbers) (apply #'+ numbers))
(defun logexpt (a b)
  "Raise A (a log-domain number) to the power B (which is not log!)"
  (* a b))

;;;; Systematic resampling

(-> systematic-resample (vector) list)
(defun systematic-resample (log-weights)
  (values
   (systematic-resample/normalized (loop with normalizer = (logsumexp log-weights)
                                         for lw across log-weights
                                         collect (exp (- lw normalizer))))))

(-> systematic-resample/normalized (list) list)
(defun systematic-resample/normalized (weights)
  (loop with n = (length weights)
        with cdf = (coerce (loop for weight in weights
                                 sum weight into cumulative
                                 collect cumulative)
                           'vector)
        with index = 0
        repeat n
        ;; Pick a starting offset into the CDF
        for u = (random (/ 1d0 n)) then (+ u (/ 1d0 n))
        ;; Step forward through the CDF
        do (loop while (and (> u (aref cdf (min index (1- n))))) do (incf index))
           (minf index (1- n))
        collect (min index (1- n))))

;;;; Probability distributions and utilities

(defun gaussian-log-likelihood (μ σ x)
  "Return the likelihood of Normal(μ,σ) at X."
  (if (plusp σ)
      (let ((z (/ (- x μ) σ)))
        (- 0
           (log σ)
           (/ (+ (* z z) (log (* pi 2))) 2)))
      most-negative-double-float))

(-> logsumexp (R*) R)
(defun logsumexp (vec)
  ;; utility for numerically stable addition of log quantities e.g. for normalization
  ;; see e.g. https://gregorygundersen.com/blog/2020/02/09/log-sum-exp/
  (let ((max (reduce #'max vec)))
    (if (sb-ext:float-infinity-p max)
        sb-ext:double-float-negative-infinity
        (loop for x across vec
              summing (if (sb-ext:float-infinity-p x) 0d0 (exp (- x max))) into acc
              finally (return (+ max (log acc)))))))
