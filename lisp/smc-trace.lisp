;;; smc-trace.lisp -- diagnostic to follow the tracks of a simulation

(defpackage #:smc-trace
  (:use #:permo-lisp #:permo-base)
  (:export #:reset #:smc-step #:*step*))
(in-package #:smc-trace)

(defparameter *z* 0)
(defparameter *step* 0)

(defun reset ()
  "Reset the SMC visualization."
  (setf *z* 0)
  (kons-9::clear-scene kons-9::*scene*))

(defun smc-step (particles)
  "Visualize the next simulation step with PARTICLES.
   Particles is a list of parameter-vectors."
  (draw-particles particles)
  (incf *step*))

(defun smc-pstep (particles)
  (let ((pdf (stm::line-posterior-pdf (dict 'smc::m (elt particles 0)
                                            'smc::c (elt particles 1)
                                            'smc::σ (elt particles 2)))))
    (flet ((height (x z)
             (* 10 (coerce (exp (funcall pdf (r x) (r z))) 'single-float))))
      (hmap #'height)
      #+nil (break)
      #+nil (format t "~&Particles =~&~A~%" particles))))

(in-package #:kons-9)

(defparameter *maxcloud* 10)
(defparameter *clouds* nil)

(defparameter *hf* nil)

(defun smc-trace::hmap (fn)
  (when (null *hf*)
    (with-clear-scene
      (setf *hf* (make-heightfield 100 100 (p! -5 0 -5) (p! 5 0 5) :name 'hf :height-fn fn))
      (add-shape *scene* *hf*)))
  (setf (height-fn *hf*) fn)
  (update-heightfield *hf*))

(defun smc-trace::draw-particles (ps)
  (setf (shading-color *drawing-settings*) (c! (lerp (/ smc-trace::*step* 20) 1d0 0d0) 0d0 0d0))
  (let ((pc (particles-to-point-cloud ps)))
    (add-cloud pc)
    (break)))

(defun add-cloud (pc)
  ;; Remove old cloud
  (when (> (length *clouds*) *maxcloud*)
    (remove-shape *scene* (alexandria:last-elt *clouds*)))
  (push pc *clouds*)
  (add-shape *scene* pc)
  (loop for i from 0 below (length *clouds*)
        for shade = (lerp (/ i *maxcloud*) 0.0 1.0)
        do (setf (shading-color *drawing-settings*) (c! shade shade 1.0))
           (allocate-point-colors (elt *clouds* i))))

(defun particles-to-point-cloud (particles)
  "Return a point-cloud mapping the two dimensions of PARTICLES onto X/Y.
   Step the Z-axis forward."
  (loop with n = (length (first particles))
        with point-array = (make-array n)
        for i below n
        do (setf (aref point-array i)
                 (p (aref (first particles) i)
                    (aref (second particles) i)
                    (aref (third particles) i)))
        finally (progn
                  (return (make-point-cloud point-array)))))
  
#+nil
(defun p (x y z)
  "Return the point (X Y Z)."
  (p:vec (coerce x 'single-float)
         (coerce y 'single-float)
         (coerce z 'single-float)))
