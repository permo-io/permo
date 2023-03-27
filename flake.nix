# Nix flake for spinning up a Permo development environment.
#
# Usage to fire up an SBCL e.g. for SLIME/SLY:
#   nix --experimental-features "nix-command flakes" develop --command sbcl
#
# Currently only supports x86-64 but not for any special reason.
{
  description = "Permo";
  inputs.nixpkgs.url = "nixpkgs";
  # nix-cl is the hip new Common Lisp packaging framework for nix.
  inputs.nix-cl.url = "github:uthar/nix-cl";
  # Just make it easy to override the McCLIM source repository.
  # e.g. with --override-input mcclim-src $HOME/git/mcclim
  # I'm often using private changes e.g. the CLIME branch. -luke
  inputs.mcclim-src = { url = "github:McCLIM/McCLIM"; flake = false; };
  outputs = { self, nixpkgs, nix-cl, mcclim-src }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      sbcl = nix-cl.packages.x86_64-linux.sbcl.withPackages (ps:
        # replace McCLIM sources in all packages that reference it
        let ps' = builtins.mapAttrs (name: pkg: if pkg.src == ps.mcclim.src
                                                then pkg.overrideAttrs(o: { src = mcclim-src; })
                                                else pkg) ps;
        in
        with ps'; [
          # language extensions
          alexandria serapeum gt
          # super userful
          cl-ppcre cl-interpol cffi
          flexi-streams
          # database
          sqlite cl-dbi dbd-sqlite3 dbd-postgres sxql
          # potentially interesting
          cl-csv vellum vellum-csv for
          cl-who cl-base64
          # high hopes that mcclim will become a key piece of the REPL workflow.
          # disabled for now because the default nix-cl package isn't right yet.
          #mcclim
        ]);
    in {
      devShells.x86_64-linux.default =
        pkgs.mkShell {
          buildInputs = [ sbcl ];
        };
    };
}
