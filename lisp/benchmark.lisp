(defpackage #:permo/benchmark
  (:nicknames #:benchmark)
  (:use #:permo #:permo-lisp)
  (:export #:benchmark-pi-circle))
(in-package #:benchmark)

(defun benchmark-pi-circle (&optional
                              (n-particles (random-in-range 1 (1+ 100000)))
                              (steps (random-in-range 1 (1+ 10000))))
  (format t "~%~S ;; host ~a~%"
          (list :n-particles n-particles
                :steps steps
                :pi (exp (permo::pi-circle ()
                                           :n-particles n-particles
                                           :steps steps)))
          (uiop:hostname)))
