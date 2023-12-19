;;; model.lisp -- statistical models

(mgl-pax:define-package #:permo-model
  (:use #:permo-lisp #:permo-base #:mgl-pax)
  (:nicknames #:model))

;;; Protocol

(defunion var
  "Concrete domain variable."
  (continuous (name string)
              (min r)
              (max r))
  (discrete (name string)
            (domain list)))

(defconstructor parameter
  "Abstract model parameter."
  (name symbol)
  (min  r)
  (max  r))

(defconstructor model
  (log-likelihood function)
  (parameters     (soft-list-of parameter))
  (variables      (soft-list-of var)))

;; print method

(defclass discrete (var)
  ((domain :initarg :domain :accessor domain)))

(defmethod print-object ((v discrete-variable) stream)
  (print-unreadable-object (v stream)
    (format stream "~A {~{~A~^,~}}" (name v) (domain v))))

;; print method

;;; Mixins


(defgeneric predict (model &rest ))
