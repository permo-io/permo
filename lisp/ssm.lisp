(mgl-pax:define-package :ssm
  (:use #:permo-lisp #:permo-base #:permo-ppl #:mgl-pax))

(in-package :ssm)

(defsection @ssm (:title "State Space Models (SSM)")
  "State Space Models track changes in state (e.g. performance) over time (e.g. across revisions.)")

(defconstructor gaussian
  (μ r)
  (σ r))

(deftype mixture ()
  '(soft-list-of gaussian))

(defun transition (mixture)
  (with-ppl ()
    (values
     (case (categorical `((:prune . ,(if (length> mixture 1) 1d0 0d0))
                          (:tweak . 10d0)
                          (:grow  . 1d0))
                        :id :move)
       (:prune
        (drop-random-element mixture))
       (:grow
        (list* (list (- (random 10d0) -5d0) ; μ
                     (random 10d0))         ; σ
               mixture))
       (:tweak
        (loop with n = (random (length mixture)) 
              for i from 0
              for x in mixture
              if (= i n)
                collect (destructuring-bind (σ μ) x
                          (list (+ σ (random 1d0))
                                (+ μ (random 1d0))))
              else
                collect x)))
     *logprob*)))
