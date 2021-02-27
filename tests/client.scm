(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (deck core net client))


(test-begin "http-client")

(test-equal "uri-parameters->string"
  "a=b&c=d"
  (uri-parameters->string '(("a" . "b") ("c" . "d"))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "http-client")

(exit exit-status)
