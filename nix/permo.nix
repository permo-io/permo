{ sbcl }:

sbcl.buildASDFSystem rec {
  pname = "permo";
  version = "git";
  src = ../lisp;
  lispLibs = import ./lisp-deps.nix sbcl.pkgs;
}
