;; Load the appropriate copy of asdf
(load (sb-ext:posix-getenv "ASDF"))
;; Preload everything needed for Permo and tooling like SLIME
(mapc #'asdf:load-system '(:permo :sb-bsd-sockets :sb-introspect))
;; Snapshot
(sb-ext:save-lisp-and-die "permo.core"
                          ;; :toplevel (lambda (&rest ignore) (princ 'hello-permo)) ;;  #'permo:toplevel
                          :executable t)
