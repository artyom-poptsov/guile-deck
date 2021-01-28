(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (deck core net http-client))


(test-begin "http-client")

(test-equal "uri-parameters->string"
  "?a=b&c=d"
  (uri-parameters->string '(("a" . "b") ("c" . "d"))))


(test-end "http-client")

(exit (= (test-runner-fail-count (test-runner-current)) 0))
