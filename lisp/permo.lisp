(mgl-pax:define-package :permo
  (:use #:permo-lisp #:permo-base #:mgl-pax))
(in-package :permo)

(defsection @permo (:title "Permo: Performance Modelling")
  "This is the top-level of the Permo docs."
  (dist::@distributions section)
  (ppl::@ppl section)
  (smc2::@smc2-manual section)
  (base::@base section)
  )
