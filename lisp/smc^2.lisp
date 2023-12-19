 (mgl-pax:define-package :smc^2
  (:use #:permo-lisp #:ppl #:permo-base #:mgl-pax))

(in-package :smc^2)

(defsection @smc^2 (:title "SMC²")
  "SMC² performs Bayesian analysis on state-space models and their fixed parameters.

  The fixed parameters are analysed by an outer SMC. Each particle represents a
  set of fixed parameters for reference by the state space model.

  The likelihood function of the outer SMC is estimated by running a nested inner SMC.")

(defclass smc^2 ())
(defclass parameter-particle ()
  (state-posterior)
  (log-marginal-likelihood))
(defclass state-particle ()
  ((parameters :initarg parameters)
   (states :initarg :states :type list)))

(defgeneric )

(defun smc)

(defun outer ()
  (loop repeat *Nθ*
        for θ = (parameter-sample-initial) then (parameter-resample-move θ)
        for S = (loop for p in θ
                      collect (list p (inner p)))
        do (weight-parameters! θ)))

(defun inner (θ)
  (loop repeat *NS*
        for S = (state-sample-initial θ) then (state-resample-move S)
        summing (weight-states! S)))

(defun state-sample-initial (θ)
  (make-instance 'state-particle
                 :states (list (random (length θ)))))

(defun state-resample-move (S)
  (loop for state in (state-resample S)
        collect (state-transition θ)))

(defun state-transition! (x)
  (when (categorical '((:same 1)
                       (:new  0.5)))
    (:same x)
    (:new (random (length θ)))))

(defun state-likelihood (θ x y)
  "Return the likelihood of θ in state X for observation Y."
  (list :l (nth x θ) y))


