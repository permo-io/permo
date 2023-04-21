;;; package.lisp -- main package definitions

;;; PERMO is a shared namespace for permo-global symbols.
(defpackage #:permo
  (:export
   ;; Types
   #:R #:R* #:P #:L #:pdf
   ;; Models
   #:defmodel #:line #:gaussian #:pi-circle
   ;; PDFs
   #:line/pdf #:uniform-mixture/pdf
   #:discretize-1d #:discretize-2d
   ;; Data import/export
   #:load-csv #:query #:schema #:dump-array
   ;; Math
   #:gaussian-log-likelihood #:logsumexp
   #:log/ #:logexpt))

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
(defpackage #:permo-user
  (:use #:permo #:permo-lisp))
