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
  inputs.nixpkgs.url = "github:lukego/nixpkgs/mgl-pax";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.mgl-pax-src.url = "github:melisgl/mgl-pax";
  inputs.mgl-pax-src.flake = false;
  outputs = { self, nixpkgs, flake-utils, mgl-pax-src }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        # customized build of sbcl
        sbcl0 = import ./nix/sbcl.nix { inherit (pkgs) wrapLisp sbcl fetchurl; };
        sbcl1 = sbcl0.withOverrides (self: super: {
          mgl-pax = super.mgl-pax.overrideLispAttrs(o: {
            src = mgl-pax-src;
            version = "trunk";
            lispLibs = o.lispLibs ++ [ super.trivial-utf-8 ];
          });
        });
        # permo lisp package
        #permo = import ./nix/permo.nix { sbcl = sbcl1; };
        # sbcl with permo available
        sbcl2 = sbcl1.withOverrides (self: super: {
          permo = import ./nix/permo.nix { sbcl = sbcl1; };
        });
        # sbcl with permo included
        sbcl = sbcl2.withPackages (ps: [ ps.permo ]);
        # executable sbcl core with permo loaded
        core = import ./nix/permo-core.nix { inherit sbcl;
                                             inherit (pkgs) lib patchelf runCommand;
                                             lispLibs = sbcl.lispLibs;
                                           };
        #
        # R
        #
        rEnv = pkgs.rWrapper.override {
          packages = with pkgs.rPackages; [
            tidyverse ggplot2 duckdb arrow cowplot gridExtra
          ];
        };
        api-html = pkgs.runCommand "api-html" {} ''
          set -e
          mkdir $out
          ${sbcl}/bin/sbcl --non-interactive \
                           --quit \
                           --eval '(require :asdf)' \
                           --eval '(require :permo)' \
                           --eval "(pax:update-asdf-system-html-docs permo::@permo :permo :target-dir \"$out/\")"
          cd $out
          ln -s permo.html index.html
        '';
      in {
        # build the 'permo' executable lisp core
        packages.default = core;
        packages.api-html = api-html;
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
