;;; package.lisp -- main package definitions

;;; PERMO-LISP is the Common Lisp dialect that's idiomatic for Permo.
(uiop:define-package #:permo-lisp
  (:nicknames #:lisp)
  (:use-reexport
   #:common-lisp #:alexandria #:serapeum
   #:trivia #:split-sequence #:cl-ppcre)
  (:shadowing-import-from #:serapeum
                          #:@)
  (:shadowing-import-from #:cl-ppcre
                          #:scan))

;;; PERMO-USER is a scratch package.
#+nil
(defpackage #:permo-user
  (:use #:permo #:permo-lisp))
