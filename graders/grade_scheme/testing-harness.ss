(library (mags testing-harness)
  (export
    mags-solution-file mags-submission-file
    define-sandbox current-sandbox
    initialize-test-suite
    test-begin test-group
    test-end test-assert test-eqv test-eq test-equal
    test-approximate test-assert test-error test-apply test-with-runner
    test-match-nth test-match-all test-match-any test-match-name
    test-skip test-expect-fail test-read-eval-string
    test-runner-group-path test-group-with-cleanup)
  (import (chezscheme)
          (srfi-64)
          #;(except (srfi-64)
                  test-equal
                  test-eq
                  test-eqv))

 
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

(define-syntax define-sandbox
  (syntax-rules ()
    [(_ (import lib ...))
     (current-sandbox
      (copy-environment
       (environment 'lib ...)
       #t))]
    [(_ (export name ...) (import lib ...) fn ...)
     (begin
       (current-sandbox (copy-environment
                         (environment 'lib ...)
                         #t))
       (define name (eval 'fn (current-sandbox))) ...)]))

(define-syntax initialize-test-suite
  (syntax-rules ()
    [(_ proc ...)
     (begin
       (test-runner-current (test-runner-simple))
       (load (mags-submission-file) (lambda (x) (eval x (current-sandbox))))
       (define proc (eval 'proc (current-sandbox))) ...)]
    [(_ sandbox proc ...)
     (begin
       (test-runner-current (test-runner-simple))
       (current-sandbox (copy-environment (environment 'sandbox) #t))
       (load (mags-submission-file) (lambda (x) (eval x (current-sandbox))))
       (define proc (eval 'proc (current-sandbox))) ...)]))


;;Let's start doing some infinite looping stuff!
#|
(define time-limit
  (make-parameter 30
    (lambda (x)
      (unless (integer? x)
        (error 'time-limit "needs a number" x))
      x))) 
(load-shared-object "libc.so.6")
(define signal/alarm 14) ;magic constant 14 from signal.h
(define $alarm (foreign-procedure "alarm" (unsigned-int) unsigned-int))
(define (with-time-limit seconds handler-thunk proc-thunk)
  (call-with-current-continuation
   (lambda (return)
     (register-signal-handler
      signal/alarm
      (lambda (x) (return (handler-thunk))))
     ($alarm seconds)
     (proc-thunk))))



;;Let's redefine test-equal with our time limit
(define-syntax test-equal
  (syntax-rules ()
      [(_ expected test-expr)
       (with-time-limit
        (time-limit)
        (lambda () (register-signal-handler $alarm (lambda (x) x)))
        (lambda () (test-assert (equal? expected test-expr))))]
      [(_ name expected test-expr)
       (with-time-limit
        (time-limit)
        (lambda () (register-signal-handler $alarm (lambda (x) x)))
        (lambda () (test-assert name (equal? expected test-expr))))]))

;;(define-syntax define-test
|#
)