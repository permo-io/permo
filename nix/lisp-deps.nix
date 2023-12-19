# Return the subset of lispPackages that Permo depends on.
lispPackages:

with lispPackages; [
  # core
  alexandria serapeum gt
  parachute mgl-pax try
  duckdb
  # dataviz
  mcclim polyclot kons-9
  # handy
  cl-ppcre cl-interpol cffi
  sqlite cl-dbi dbd-sqlite3 dbd-postgres sxql
  cl-csv for
  cl-who cl-base64
  adopt adopt-subcommands
  vellum vellum-csv vellum-postmodern
]
