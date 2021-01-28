(define-module (matrix core types matrix-content-uri)
  #:use-module (oop goops)
  #:export (<matrix-content-uri>
            matrix-content-uri-server
            matrix-content-uri-protocol
            matrix-content-uri-media-id))


(define-class <matrix-content-uri> ()
  ;; <string>
  (server
   #:init-value   #f
   #:init-keyword #:server
   #:getter       matrix-content-uri-server)

  ;; <string>
  (protocol
   #:init-value   #f
   #:init-keyword #:protocol
   #:getter       matrix-content-uri-protocol)

  ;; <string>
  (media-id
   #:init-value   #f
   #:init-keyword #:media-id
   #:getter       matrix-content-uri-media-id))



