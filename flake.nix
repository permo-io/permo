# Nix flake for spinning up a Permo development environment.
#
# Usage:
#   nix run
#     ... to start an SBCL REPL with Permo loaded.
#   nix build
#     ... to create result/bin/permo, an executable sbcl image with permo loaded.
#
# Must have nix "flakes" enabled in configuration or else tweak commands like:
#   nix --experimental-features "nix-command flakes" develop

{
  description = "Permo: performance testing robot";
  inputs.nixpkgs.url = "github:lukego/nixpkgs/lisp-kons-9";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        # customized build of sbcl
        sbcl0 = import ./nix/sbcl.nix { inherit (pkgs) wrapLisp sbcl fetchurl; };
        # permo lisp package
        permo = import ./nix/permo.nix { sbcl = sbcl0; };
        # sbcl with permo available
        sbcl1 = sbcl0.withOverrides (self: super: { inherit permo; });
        # sbcl with permo included
        sbcl = sbcl1.withPackages (ps: [ ps.permo ]);
        # sbcl with dependencies of permo
        dev = sbcl0.withPackages (ps: import nix/lisp-deps.nix ps);
        # executable sbcl core with permo loaded
        core = import ./nix/permo-core.nix { inherit sbcl;
                                             inherit (pkgs) lib patchelf runCommand;
                                             lispLibs = dev.lispLibs;
                                           };
        #
        # R
        #
        rEnv = pkgs.rWrapper.override {
          packages = with pkgs.rPackages; [
            tidyverse ggplot2 duckdb arrow cowplot gridExtra
          ];
        };
      in {
        # build the 'permo' executable lisp core
        packages.default = core;
        apps.default = { type = "app"; program = "${core}/bin/permo"; };
        apps.R       = { type = "app"; program = "${rEnv}/bin/R"; };
        apps.benchmark-pi-circle = {
          type = "app";
          program = (let script = pkgs.writeScript "pi-circle" ''
                                    ${pkgs.linuxPackages.perf}/bin/perf stat -x \; \
                                    ${core}/bin/permo --script <<EOF
                                    (setf *random-state* (make-random-state t))
                                    (time (benchmark:benchmark-pi-circle))
                                    EOF
                                  ''; in
                       "${script}");
        };
      }
    );
}
