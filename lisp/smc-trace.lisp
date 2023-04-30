;;; trail.lisp -- diagnostic to follow the tracks of a simulation

(defpackage #:smc-trace
  (:use #:permo #:permo-lisp))
(in-package #:smc-trace)

(defun smc-step (particles)
  (draw-particles particles))

(in-package #:kons-9)

(defun smc-trace::draw-particles (ps)
  (with-clear-scene
    (add-shape *scene* (particles-to-point-cloud ps))))

(defun particles-to-point-cloud (ps)
  (loop with n = (length (first ps))
        with point-array = (make-array n)
        for i below n
        do (setf (aref point-array i)
                 (list (aref (first ps) i) (aref (second ps) i) 0d0))
        finally (return (make-point-cloud point-array))))
  
