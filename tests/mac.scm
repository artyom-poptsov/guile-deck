(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (rnrs bytevectors)
             (oop goops)
             (deck core mac))


(test-begin "mac")

(test-equal "bin->hex"
  "68656c6c6f"
  (bin->hex (string->utf8 "hello")))


(test-end "mac")

(exit (= (test-runner-fail-count (test-runner-current)) 0))
