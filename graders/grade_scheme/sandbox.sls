(library
  (mags sandbox)
  (export 
    run-sandbox-tests
    &sandbox
    &timeout
    &illegal-term
    &sandbox-i/o-error
    &unbound-term
    sandbox-condition
    sandbox-condition?
    timeout
    timeout?
    illegal-term
    illegal-term?
    sandbox-i/o-error
    sandbox-i/o-error?
    unbound-term
    unbound-term?
    timeout-engine
    sandbox-i/o-error-port
    illegal-term-name
    eval/sandbox
    load/sandbox
    library/sandbox
    flag-error flag-warning import
    sandbox)
  (import (chezscheme) (srfi :64))




  ;;; Conditions
  ;;
  ;; These conditions are raised when certain problems arise in the evaluation of
  ;; terms inside the sandbox environment. There are four conditions:
  ;;
  ;; 1) &sandbox
  ;; 2) &timeout
  ;; 3) &sandbox-i/o-error
  ;; 4) &illegal-term
  ;;
  ;; Each has its own special occasion for use, as well as constructors,
  ;; accessors, and predicates.

  ;; |&sandbox| is the parent condition for all of the following sandbox
  ;; conditions. The constructor is |sandbox-condition| and predicate
  ;; |sandbox-condition?|. It takes no arguments.

  (define-condition-type
    &sandbox
    &condition
    sandbox-condition
    sandbox-condition?)

  ;; |&timeout| is a child condition of the parent |&sandbox|. This condition is
  ;; raised when the evaluation of a certain procedure runs out of time. It
  ;; takes one argument, |timeout-engine| which represents the unique engine
  ;; from which this timeout condition was raised. 
  ;; Predicate: |timeout?|, Constructor: |(timeout timeout-engine)|.

  (define-condition-type
    &timeout
    &sandbox
    timeout
    timeout?
    (engine timeout-engine))

  ;;  |&sandbox-i/o-error| is a child condition of the parent type |&sandbox|.
  ;;  This condition is raised when there arises an input/output error or
  ;;  filesystem error when the sandbox loads or writes files. It takes one
  ;;  argument, |sandbox-i/o-error-port|, i.e. the port where the error occured.
  ;;  Predicate: |sandbox-i/o-error?|, Constructor: |(sandbox-i/o-error port)|.
  (define-condition-type
    &sandbox-i/o-error
    &sandbox
    sandbox-i/o-error
    sandbox-i/o-error?
    (port sandbox-i/o-error-port))

  ;; |&illegal-term| is a child condition of the parent type |&sandbox|, This
  ;; conditions is raised when there arises a term deemed illegal that is
  ;; evaluated in the sandbox. It takes one argument |illegal-term-name| that is
  ;; the term deemed illegal by the sandbox.
  ;; Predicate: |illegal-term?|, Constructor: |(illegal-term name)|.
  (define-condition-type
    &illegal-term
    &sandbox
    illegal-term
    illegal-term?
    (name illegal-term-name))

  ;; |&unbound-term| is a child condition of the parent |&sandbox|. This
  ;; condition is raised when the evaluation of a procedure expected to be found
  ;; is not found in the code within the sandbox. It takes one argument, the
  ;; |unbound-term-name|, the name of the term which was determined to be unbound.
  ;; Predicate: |unbound-term?|, Constructor: |(unbound-term name)|.
  (define-condition-type
    &unbound-term
    &sandbox
    unbound-term
    unbound-term?
    (name unbound-term-name))



  )
