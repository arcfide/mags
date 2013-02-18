(define-sandbox (a2 sandbox)
  (export-all)
  (import (chezscheme)))

(define-from-submission plus1)

(test-group "plus1"
  (test-equal 2 (plus1 1))
  (test-equal 0 (plus1 -1))
  (test-equal 29 (plus1 (plus1 (plus1 26)))))

