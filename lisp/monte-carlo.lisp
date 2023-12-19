(mgl-pax:define-package :monte-carlo
  (:use #:permo-lisp #:ppl #:permo-base #:mgl-pax))

(in-package :monte-carlo)

(defsection @monte-carlo (:title "Monte Carlo")
  "Monte Carlo simulations."
  (simulation class)
  (particle class)
  (@importance-sampling section))

(defclass simulation ()
  ((logml :accessor logml :initform 1d0 :type R
          :documentation "Log marginal likelihood.")
   (particles :initarg :particles :accessor particles)))

(defclass particle ()
  ((log-weight :initarg :weight :accessor weight :type R)
   (normalized-weight :initarg normalized-weight :accessor normalized-weight :type R)
   (parameter :initarg :parameter)))

(defgeneric sample (distribution))

(defmethod sample ((x sequence))
  (values (random-elt x)
          (log (/ 1 (length x)))))

(defsection @importance-sampling (:title "IS: Importance Sampling"))

(defclass importance-sampling (simulation)
  
  )

(defun is (f log-likelihood proposal-dist n)
  (loop repeat n
        for (x proposal-log-likelihood) = (multiple-value-list (sample proposal-dist)) 
        for importance-weight = (log* (funcall log-likelihood x) proposal-log-likelihood)
        summing (* (funcall f sample) (/ (funcall likelihood) )

(defun is2 (f n proposal.d log-likelihood)
  (loop with sim = (make-instance 'importance-sampling
                                  :particles (loop repeat n
                                                   collect (is-initial-particle proposal.d)))
        
        )
  (let* ((particles (coerce (loop repeat n collect (make-instance 'is-particle)) 'vector)))))

(defclass is-particle (particle)
  ((proposal.ll :initarg :proposal.ll :accessor proposal.ll)))

(defun is-initial-particles (n proposal.d)
  (loop repeat n
        collect))

(defun is-initial-particle (proposal.d)
  (receive (x ll) (sample proposal.d)
    (make-instance 'is-particle
                   :proposal.ll ll
                   :parameter x)))

(defun is2-simulation (n proposal.d)
  (lret ((sim (make-instance 'importance-sampling
                             :particles (loop repeat n collect (funcall proposal.d)))))))

  (make-instance )
  (make-simulation n (lambda () ())))

(defun make-simulation (class n particle-fn)
  (lret ((sim (make-instance class)
              :particles (make-array n)))
    (loop for i below n
          do (setf (aref (particles sim)))

  (lret ((sim (make-instance 'simulation
                             :particles (loop))))
    )


  (loop with particles = (loop repeat n
                               collect (receive (x ll) (sample proposal.d)
                                         (make-instance 'particle
                                                        :log-weight ll
                                                        :parameters (funcall proposal))))
        
  ))

(defclass is-particle (particle)
  (log-importance-weight :initarg :log-importance-weight :accessor log-importance-weight))

(defun is2-init (particles proposal.d)
  (loop for p across particles
        do (receive (x ll) (sample proposal.d)
             (make-instance 'is-particle )
             )
do ())

(defsection @sequential-importance-sampling (:title "SIS: Sequential Importance Sampling"))

(defun sis (f log-likelihood ))

(defsection @sequential-importance-sampling-resampling (:title "SIR: Sequential Importance Resampling"))

(defsection @sequential-monte-carlo (:title "SMC: Sequential Monte Carlo"))

(defsection @sequential-monte-carlo-squared (:title "SMC²: Sequential Monte Carlo 'Squared'"))

(defsection @nested-sequential-monte-carlo (:title "NSMC: Nested Sequential Monte Carlo"))

  

(defmethod log-marginal-likelihood-estimate (mc))
(defmethod log-marginal-likelihood-error (mc))
(defmethod parameters (mc))

(defmethod rejuvenate (mc))
(defmethod effective-sample-size (mc))
(defmethod resample (mc))

(defmethod normalized-weights (mc))
(defmethod unnormalized-weights (mc))





