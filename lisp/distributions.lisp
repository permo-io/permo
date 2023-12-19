(mgl-pax:define-package :permo-distributions
  (:use #:permo-lisp #:permo-base #:mgl-pax)
  (:nicknames :distributions :dist)
  (:export #:gaussian-sample #:gaussian-log-likelihood))
(in-package :dist)

(defsection @distributions (:title "Probability distributions")
  "Sample from common probability distributions and calculate the log-likelihood
  of values conditional on parameters."
  (uniform-sample             function)
  (uniform-log-likelihood     function)
  (gaussian-sample            function)
  (gaussian-log-likelihood    function)
  (categorical-sample         function)
  (categorical-log-likelihood function)
  (bernoulli-sample           function)
  (bernoulli-log-likelihood   function)
  (exponential-sample         function)
  (exponential-log-likelihood function)
  (laplace-sample             function)
  (laplace-log-likelihood     function)
  (uniform-discrete-sample         function)
  (uniform-discrete-log-likelihood function)
  )

(defun gaussian-sample (μ σ)
  "Sample from a Gaussian (normal) distribution with mean μ and stddev σ."
  (+ μ (* (gaussian-random) σ)))

(-> gaussian-log-likelihood (r r r) r)
(defun gaussian-log-likelihood (μ σ x)
  "Return the log likelihood of Normal(μ,σ) at X."
  (if (plusp σ)
      (let ((z (/ (- x μ) σ)))
        (- 0d0
           (log σ)
           (/ (+ (* z z) (log (* pi 2))) 2)))
      most-negative-double-float))

(-> uniform-sample (r r) t)
(defun uniform-sample (min max)
  "Sample a uniform random number between MIN and MAX.

  ```cl-transcript (:dynenv pax-dynenv)
  (values (uniform-sample 0d0 1d0)
          (uniform-sample 1d0 100d0)
          (uniform-sample -100d0 0d0))
  => 0.09762725147687568d0
  => 43.607497770603814d0
  => -79.44728298705866d0
  ```"
  (if (= min max)
      min
      (+ min (random (abs (- min max))))))

(-> uniform-log-likelihood (r r r) r)
(defun uniform-log-likelihood (min max x)
  "Return the log likelihood of Uniform(MIN,MAX) at X."
  (cond ((= min max)
         (log 1d0))
        ((<= min x max)
         (log (/ 1 (abs (- min max)))))
        (t
         most-negative-double-float)))

(-> categorical-sample (sequence) t)
(defun categorical-sample (category-weight-alist)
  "Draw from a weighted categorical distribution.

  ```cl-transcript (:dynenv pax-dynenv)
  (~> (loop repeat 10000
            collect (categorical-sample '(a b c) '(100d0 10d0 1d0)))
      (frequencies)
      (hash-table-alist)
      (sort #'string< :key (compose #'symbol-name #'car)))
    => ((A . 8988) (B . 923) (C . 89))
  ```"
  (let ((categories (mapcar #'car category-weight-alist))
        (weights (mapcar #'cdr category-weight-alist)))
    (nth (count-if (fuel (random (reduce #'+ weights))) weights)
         categories)))

(defun categorical-log-likelihood (category-weight-alist x)
  "Return the log-likelihood of drawing X from a categorical distribution with CATEGORY-WEIGHT-ALIST.

  ```cl-transcript
  (let ((cw '((a . 100d0) (b . 10d0) (c . 1d0))))
    (values (categorical-log-likelihood cw 'a)
            (categorical-log-likelihood cw 'b)
            (categorical-log-likelihood cw 'c)))
  => -0.10436001532424276d0
  => -2.4069451083182885d0
  => -4.709530201312334d0
  ```"
  (loop for (c . w) in category-weight-alist
        with total-weight = (reduce #'+ category-weight-alist :key #'cdr)
        when (equal c x)
          do (return (log (/ w total-weight)))
        finally (error "Category ~A not recognized" x)))

(-> uniform-discrete-sample (N>0) N>=0)
(defun uniform-discrete-sample (n)
  (random n))

(-> uniform-discrete-log-likelihood (N>0 N>=0) L)
(defun uniform-discrete-log-likelihood (n x)
  (if (<= 0 x (1- n))
      (log (/ 1d0 n))
      sb-ext:double-float-negative-infinity))

(-> bernoulli-sample (p) (member t nil))
(defun bernoulli-sample (p)
  (< (random 1d0) p))

(-> bernoulli-log-likelihood (p (member t nil)) r)
(defun bernoulli-log-likelihood (p x)
  (log (if x p (- 1 p))))

(-> exponential-sample (r>0) r) 
(defun exponential-sample (rate)
  (- (/ (log (random 1d0)) rate)))

(-> exponential-log-likelihood (r>0 r) r)
(defun exponential-log-likelihood (rate x)
  (if (minusp x)
      sb-ext:double-float-negative-infinity
      (- (log rate) (* rate x))))

(-> laplace-sample (r r>0) r)
(defun laplace-sample (location scale)
  (+ location (* (exponential-sample scale)
                 (if (zerop (random 2)) 1 -1))))

(-> laplace-log-likelihood (r r>0 r) r)
(defun laplace-log-likelihood (location scale x)
  (log/ (exponential-log-likelihood scale (abs (- x location)))
        (log 2d0)))
