(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 regex)
             (oop goops)
             (deck core types matrix-content-uri))


(test-begin "matrix-content-uri")

(test-assert "matrix-content-uri?"
  (and (matrix-content-uri? (make <matrix-content-uri>
                              #:server   "matrix.org"
                              #:protocol "mxc"
                              #:media-id "test"))
       (not (matrix-content-uri? "not a content URI"))))

(test-assert "display"
  (let ((output (with-output-to-string
                  (lambda ()
                    (display (make <matrix-content-uri>
                               #:server   "matrix.org"
                               #:protocol "mxc"
                               #:media-id "test"))))))
    (string-match "#<matrix-content-uri mxc://matrix.org/test [0-9a-z]+>"
                  output)))

(test-equal "matrix-content-uri-protocol"
  "mxc"
  (matrix-content-uri-protocol "mxc://example.org/test"))

(test-equal "matrix-content-uri-server"
  "example.org"
  (matrix-content-uri-server "mxc://example.org/test"))

(test-equal "matrix-content-uri-server"
  "test"
  (matrix-content-uri-media-id "mxc://example.org/test"))

(test-equal "string->matrix-content-uri"
  (make <matrix-content-uri>
             #:server   "matrix.org"
             #:protocol "mxc"
             #:media-id "test")
  (string->matrix-content-uri "mxc://matrix.org/test"))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "matrix-content-uri")

(exit exit-status)
