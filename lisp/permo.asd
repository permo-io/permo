(asdf:defsystem "permo"
  :depends-on (#:gt #:duckdb #:cl-interpol)
  :components ((:file "smc")
               (:file "benchmark")))
