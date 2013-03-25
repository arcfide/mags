(library (mags testing-harness)
  (export
   test-suite
   test-group
   test-assert
   test-equal
   test-set
   test-eqv
   test-eq
   test-approx
   test-load
   test-begin
   test-end
   test-entry
   define-equality-test
   current-time-limit)
  (import
   (chezscheme)
   (except
    (rename (srfi :64)
            (test-assert srfi:test-assert)
            (test-group srfi:test-group)
            (test-runner-current current-test-runner))
    test-equal
    test-eqv
    test-eq))

  )