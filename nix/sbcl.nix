# Customized sbcl build for Permo
{ wrapLisp, sbcl, fetchurl }:

let
  patches = [
    # sb-perf profiling support
    (fetchurl { url = https://github.com/phmarek/sbcl/commit/c82ecd251619089d114b87a4bd366f36583c6da3.diff;
                sha256 = "02jr14kyirmi482c1hsv4l7lrvasj2pclhxh9wk6mccqalg9i4yx";
              })
  ];
  sbcl' = sbcl.overrideAttrs (oa:
    {
      patches = oa.patches ++ patches;
      version = "${oa.version}-permo";
    });
in

wrapLisp { pkg = sbcl'; faslExt = "fasl"; }
