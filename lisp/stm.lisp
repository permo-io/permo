;;; stm.lisp -- short-term memory, ephemeral state via in-memory duckdb
(defpackage #:permo/stm
  (:use #:gt)
  (:shadow #:query)
  (:nicknames #:stm)
  (:export #:load-csv
           #:categorical-groups))
(in-package #:permo/stm)
(named-readtables:in-readtable :interpol-syntax)

;;;; State

(defparameter *observations/step* 100
  "Number of observations to use in each inference step.")

;;;; Initialization

(defun init ()
  (ddb:initialize-default-connection)
  (values))

;;; API
(defun load-csv (filename)
  (init)
  (ddb:query #?"CREATE TABLE observation AS SELECT * FROM read_csv_auto('${filename}')" nil))

;;;; Utility functions

(defun query (query &rest parameters)
  (alist-hash-table (ddb:query query parameters) :test 'equal))

(defun schema ()
  (loop with raw = (query #?"SELECT column_name, data_type \
                               FROM information_schema.columns \
                               WHERE table_name == 'observation'")
        for name across (@ raw "column_name")
        for type across (@ raw "data_type")
        collect (cons name type)))

;;;; Categorical variables

(defun categorical-fixed-variables ()
  "Return the list of names of fixed variables that are categorical."
  (loop for (name . type) in (schema)
        when (string= type "VARCHAR")
          collect name))

(defun levels (variable)
  "Return the distinct levels of the categorical VARIABLE."
  (coerce (@ (query #?"SELECT DISTINCT ${variable} AS levels FROM observation")
             "levels") 'list))

;;; API
(defun categorical-groups ()
  "Return a list of (CATEGORY . LEVELS) for all categorical observation variables."
  (loop for name in (categorical-fixed-variables)
        collect (cons name (levels name))))

;; XXX hard-coded
(defun observations (host &optional (step 0))
  (query #?"SELECT * FROM observation WHERE host == '${host}' \
            LIMIT ? OFFSET ?" *observations/step* (* step *observations/step*)))

;;; SMC inference integration

(defun test-infer-1 (host variables &key (step 0))
  (loop for v in variables
        collect (cons v
                      (nth-value 0
                       (permo::gaussian (loop for obs across (@ (observations host step) v)
                                              collect (list (coerce obs 'double-float)))
                                        :n-particles 500 :steps 100)))))

(defun test-infer-2 (host variables &key (step 0))
  (loop for v in variables
        collect (cons v
                      (multiple-value-list
                       (permo::line (loop for obs0 across (@ (observations host step) v)
                                          for obs1 across (@ (observations host step) "steps")
                                          ;;for obs2 across (@ (observations host step) "n_particles")
                                          collect (list (coerce (* obs1 1 #+nil obs2) 'double-float)
                                                        (coerce obs0 'double-float)))
                                    :n-particles 100 :steps 1000)))))

#+nil
(defun test-infer-3 (host variables &key (step 0))
  (loop for v in variables
        collect (cons v
                      (multiple-value-list
                       (permo::line (loop for x across (@ (observations host step) v)
                                          for y across (@ (observations host step) "cpu_cycles")
                                          collect (list (permo::r x) (permo::r y))
                                                        (coerce obs0 'double-float))
                                    :n-particles 1000 :steps 1000)))))

;;;; Export data for diagnostics (etc)

(defun dump-array (filename columns array &key (table-name "data"))
  (assert (= (length columns) (1+ (array-rank array))))
  (ddb:with-open-database (db)
    (ddb:with-open-connection (duckdb:*connection* db)
      (query (dump-array/create-table table-name columns))
      (ddb:with-appender (appender table-name)
        (loop for index from 0
              while (< index (apply #'* (array-dimensions array)))
              do (ddb:append-row appender
                                 (append (mapcar #'permo::R (array-index-row-major array index))
                                         (list (row-major-aref array index))))))
      (query #?"COPY ${table-name} TO '${filename}'"))))

(defun dump-array/create-table (table-name columns)
  (format nil #?"CREATE OR REPLACE TABLE ${table-name} (~{~a DOUBLE~^, ~})" columns))
