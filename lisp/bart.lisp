;;;; bart.lisp -- Bayesian Additive Regression Trees

(defpackage :bart
  (:use #:cl #:serapeum :ppl))
(in-package :bart)

(defunion node
  (leaf   (value double-float))
  (branch (var   t)
          (cut   (double-float 0d0 1d0))
          (left  node)
          (right node)))

(defstruct bart
  (nodes nil :type (vector node)))

;;;; Tree model

(defparameter *alpha* 0.95d0)
(defparameter *beta* 2d0)
(defparameter *kappa* 2d0)
(defparameter *ntrees* 50)
(defparameter *scale* 10d0)
(defparameter *dim* nil)

(defun tree (*dim*)
  (tree-model))

(defun tree-model (&optional (depth 0) bounds)
  (if (bernoulli (* *alpha* (expt *beta* (- depth))) :id :split)
      (branch-model depth bounds)
      (leaf-model)))

(defun branch-model (depth bounds)
    (mvlet* ((var (uniform-discrete *dim* :id :var))
             (min max (apply #'values
                             (or (cdr (assoc var bounds))
                                 '(0d0 1d0))))
             (cut (uniform min max :id :cut)))
      (branch var
              cut
              (tree-model (1+ depth) (acons var (list min cut) bounds))
              (tree-model (1+ depth) (acons var (list cut max) bounds)))))

(defun leaf-model ()
  ;; Gaussian prior on leaf value but laplace likelihood of observations
  (leaf (gaussian 0.2d0 0.1d0)))

(defun prediction (tree x)
  (match-of node tree
    ((leaf value) value)
    ((branch var cut left right)
     (prediction (if (< (nth var x) cut)
                     left
                     right)
                 x))))

(defun tree-observe (tree xs ys)
  (loop for x in xs
        for y in ys
        do (laplace (prediction tree x) *scale*
                    :x y)))


(defun tree-likelihood (tree xs ys)
  (with-probability ()
    (tree-observe tree xs ys)
    *logprob*))

