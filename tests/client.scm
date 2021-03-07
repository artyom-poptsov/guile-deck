(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (web uri)
             (oop goops)
             (deck core net client))


(test-begin "http-client")

(test-equal "uri-parameters->string"
  "a=b&c=d"
  (uri-parameters->string '(("a" . "b") ("c" . "d"))))

(test-equal "client-build-uri"
  (build-uri 'http
             #:host "example.org"
             #:port 80
             #:path "/test"
             #:query "a=b&c=d")
  (let ((client (make <client>
                  #:server (build-uri 'http
                                      #:host "example.org"
                                      #:port 80
                                      #:path "/"))))
    (client-build-uri client "/test" '(("a" . "b") ("c" . "d")))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "http-client")

(exit exit-status)
