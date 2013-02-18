(library (mags testing-harness)
  (export
    mags-solution-file mags-submission-file
    current-sandbox
    initialize-test-suite
    test-begin test-group
    test-end test-assert test-eqv test-eq test-equal
    test-approximate test-assert test-error test-apply test-with-runner
    test-match-nth test-match-all test-match-any test-match-name
    test-skip test-expect-fail test-read-eval-string
    test-runner-group-path test-group-with-cleanup)
  (import (chezscheme) (srfi-64))

(define mags-solution-file
  (make-parameter ""
    (lambda (x)
      (unless (string? x)
        (error 'mags-solution-file "needs a pathname" x))
      x)))

(define mags-submission-file
  (make-parameter ""
    (lambda (x)
      (unless (string? x)
        (error 'mags-submission-file "needs a pathname" x))
      x)))

(define current-sandbox
  (make-parameter
    (lambda (x)
      (unless (environment? x)
        (error 'current-sandbox "environment required" x))
      x)))

(define-syntax initialize-test-suite
  (syntax-rules ()
    [(_ sandbox proc ...)
     (begin
       (test-runner-current (test-runner-simple))
       (current-sandbox (copy-environment (environment 'sandbox) #t))
       (load (mags-submission-file) (lambda (x) (eval x (current-sandbox))))
       (define proc (eval 'proc (current-sandbox))) ...)]))

)