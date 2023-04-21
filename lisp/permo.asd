(asdf:defsystem "permo"
  :depends-on (#:alexandria #:serapeum #:cl-ppcre #:trivia #:duckdb #:cl-interpol)
  :components ((:file "packages")
               (:file "smc")
               (:file "stm")
               (:file "benchmark")))
