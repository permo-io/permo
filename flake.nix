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
  inputs.nixpkgs.url = "nixpkgs";
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
                                               inherit (pkgs) runCommand; };
      in {
        # build the 'permo' executable lisp core
        packages.default = core;
        apps.default = { type = "app"; program = "${core}/bin/permo"; };
      }
    );
}
