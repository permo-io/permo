(defpackage #:smc-trace3d
  (:use #:permo #:permo-lisp)
  (:export #:reset #:smc-step #:*step*))
(in-package #:smc-trace3d)

(defvar *step* nil)


