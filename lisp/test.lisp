;;;; test.lisp -- unit tests for bart

(defpackage :bart-test
  (:use #:cl #:serapeum #:parachute)
  (:shadowing-import-from :parachute true))

(in-package :bart-test)

;; (test 'create)
(define-test create
  (bart::make-bart :nodes (vector (bart::leaf 1d0))))

