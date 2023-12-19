;;; inference.lisp -- Bayesian inference using Sequential Monte Carlo simulation

(pax:define-package #:permo-inference
  (:use #:permo-lisp)
  (:nicknames #:inference))
(in-package #:inference)

(define-symbol-macro × (error "undefined value"))

(pax:defsection @particles (:title "Particles")
  "Particles (a collective noun) is a probability distribution approximated by a set of weighted particles."
  (particles pax:class)
  (make-particles pax:function))

(defclass particles ()
  (values))

(defstruct (particles (:constructor %make-particles))
  parameter-names
  parameter-values)

(defun map-particles)

(defgeneric map-particles (fn particles)
  (:documentation "Call function (FN &rest PARAMETER-VALUES) for each of PARTICLES."))

(pax:defsection @particles-memory (:title "Lisp heap-allocated particles")
  "Particles with values stored in double-float vectors on the Lisp heap.")

(defstruct (lisp-particles (:include particles) (:constructor %make-lisp-particles))
  (values (required-argument) :type (soft-list-of (vector double-float))))

(defun make-lisp-particles (parameters)
  (%make-lisp-particles :parameters parameters
                        :values (loop repeat (length parameters)
                                      collect (make-array 0 :element-type 'double-float :adjustable t))))

(defmethod map-particles (fn (ps lisp-particles))
  (with-slots (parameter-names particles) ps
    (loop for p from 0 below (length (first particles))
          do (apply fn (loop for vec in values
                             collect (elt p vec))))))

