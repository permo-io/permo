# Return the subset of lispPackages that Permo depends on.
lispPackages:

with lispPackages; [
  # core
  alexandria serapeum gt
  parachute
  duckdb
  # handy
  cl-ppcre cl-interpol cffi
  sqlite cl-dbi dbd-sqlite3 dbd-postgres sxql
  cl-csv for
  cl-who cl-base64
  adopt adopt-subcommands
]
