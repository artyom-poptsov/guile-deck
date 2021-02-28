(define-module (deck core types turn-server)
  #:use-module (oop goops)
  #:use-module (deck core types matrix-id)
  #:export ())


;; See <https://matrix.org/docs/api/client-server/#!/VOIP/getTurnServer>
(define-class <turn-server> ()
  ;; <string>
  (password
   #:init-value   #f
   #:init-keyword #:password
   #:getter       turn-server-password)

  ;; <number>
  (ttl
   #:init-value   #f
   #:init-keyword #:ttl
   #:getter       turn-server-ttl)

  ;; <list>
  (uris
   #:init-value   '()
   #:init-keyword #:uris
   #:getter       turn-server-uris)

  ;; <string>
  (username
   #:init-value   #:f
   #:init-keyword #:username
   #:getter       #:username))



(define-method (display (turn-server <turn-server>) (port <port>))
  (format port "#<turn-server ~a>"
          (number->string (object-address pipe) 16)))

(define-method (write (turn-server <turn-server>) (port <port>))
  (display turn-server port))

(define-method (display (turn-server <turn-server>))
  (next-method)
  (display turn-server (current-output-port)))

(define-method (write (turn-server <turn-server>))
  (next-method)
  (display turn-server (current-output-port)))


