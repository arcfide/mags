(library-group
  (include "srfi-64.sls")
  (include "testing-harness.ss"))

(let ()
  (define (main . args)
    (unless (= 3 (length args))
      (errorf 'scheme-grader
        "scheme-grader : test-suite solution submission"))
    (let-values ([(suite sol sub) (apply values args)])
      (let ([suite-env (make-suite-environment sol sub)])
        (load suite (lambda (x) (eval x suite-env))))))

  (define (make-suite-environment solution submission)
    (let ([env (copy-environment
                 (environment '(chezscheme) '(mags testing-harness))
                 #t)])
      (eval `(mags-solution-file ,solution) env)
      (eval `(mags-submission-file ,submission) env)
      env))

  (scheme-start main))