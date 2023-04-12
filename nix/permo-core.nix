{ lib, patchelf, runCommand, sbcl, lispLibs }:

let
  inherit (lib.lists) flatten;
  inherit (lib.strings) makeLibraryPath;
  libraryPath = makeLibraryPath (flatten (map (p: p.nativeLibs) lispLibs));
in

runCommand "permo-sbcl-core" { buildInputs = [ sbcl ]; }
  ''
    sbcl --load ${./permo-core-init.lisp}
    mkdir -p $out/bin $out/lib
    cp permo.core $out/lib/
    cat > $out/bin/permo <<EOF
    #!/usr/bin/env bash
    export LD_LIBRARY_PATH=${libraryPath}:$LD_LIBRARY_PATH
    ${sbcl}/bin/sbcl --core "$out/lib/permo.core" "\$@"
    EOF
    chmod +x $out/bin/permo
  ''
