(mgl-pax:define-package #:permo-base
  (:use #:permo-lisp #:mgl-pax)
  (:nicknames #:base))
(cl:in-package #:permo-base)

(defsection @base (:title "Basic utilities")
  "Blurble."
  (R type)
  (R function)
  (R>0 type)
  (R>=0 type)
  (N type)
  (N>0 type)
  (N>=0 type)
  (R* type)
  (R* function)
  (R** type)
  (P type)
  (L type)
  (pdf type)
  (∅ variable)
  (⊥ symbol-macro)
  (logsumexp function)
  (log/ function)
  (logexpt function)
  (pax-dynenv function))

(deftype R    (&rest args) "Real number (double-float.)"   `(double-float ,@args))
(deftype R>0  ()           "Real (double-float) > 0."      `(R 2.2250738585072014d-308 *)) 
(deftype R>=0 ()           "Real (double-float) ≥ 0."      `(R 0d0 *)) 
(deftype N    (&rest args) "Integer ≥ 0."                  `(integer ,@args)) 
(deftype N>=0 ()           "Integer ≥ 0."                  `(integer 0 *)) 
(deftype N>0  ()           "Integer > 0."                  `(integer 1 *)) 
(deftype R*   ()           "Real (double-float) vector."   '(simple-array R  (*)))
(deftype R**  ()           "Real (double-float) 2D array." '(simple-array R  (* *)))
(deftype P    ()           "Probability: real in [0,1]."   '(R 0d0 1d0))
(deftype L    ()           "Log-likelihood (real.)"        'R)
(deftype pdf  ()           "Probability density function." '(function (&rest R) R))

(def ∅ most-negative-double-float
  "Log-probability of an impossible event. (exp ⊥) => 0d0.")

(define-symbol-macro ⊥ (error "⊥: Undefined value"))

(-> R (number) R)
(defsubst R (number)
  "Coerce NUMBER to type R (double-float)."
  (coerce number 'R))

(-> R* (sequence) R*)
(defsubst R* (numbers)
  "Return NUMBERS in a simple-vector of double-float (type R*)."
  (map 'R* (op (coerce _ 'double-float)) numbers))

(-> logsumexp (R*) R)
(defun logsumexp (vec)
  ;; utility for numerically stable addition of log quantities e.g. for normalization
  ;; see e.g. https://gregorygundersen.com/blog/2020/02/09/log-sum-exp/
  (let ((max (the R (reduce #'max vec))))
    (if (sb-ext:float-infinity-p max)
        sb-ext:double-float-negative-infinity
        (loop for x across vec
              summing (if (sb-ext:float-infinity-p x) 0d0 (exp (- x max))) into acc of-type R
              finally (return (the R (+ max (log acc))))))))

;; Helpers for arithmetic in the log domain.
;; Used sparingly when it helps to clarify intent.
(-> log/ (r r) r)
(defun log/ (a b) (- a b))
;;(defun log* (&rest numbers) (apply #'+ numbers))
(-> logexpt (r r) r)
(defun logexpt (a b)
  "Raise A (a log-domain number) to the power B (which is not log!)"
  (* a b))

(defun pax-dynenv (transcribe)
  (let ((*random-state* (sb-ext:seed-random-state 0)))
    (funcall transcribe)))
