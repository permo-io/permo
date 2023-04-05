{ runCommand, sbcl }:

runCommand "permo-sbcl-core" { buildInputs = [ sbcl ]; }
  ''
    sbcl --load ${./permo-core-init.lisp}
    mkdir -p $out/bin
    cp permo.core $out/bin/permo
  ''
