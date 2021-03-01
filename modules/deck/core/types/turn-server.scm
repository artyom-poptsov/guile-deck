(define-module (deck core types turn-server)
  #:use-module (oop goops)
  #:use-module (deck core types matrix-id)
  #:export (<turn-server>
            turn-server?
            turn-server-password
            turn-server-ttl
            turn-server-uris
            turn-server-username
            alist->turn-server))


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
   #:getter       turn-server-username))



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



(define (turn-server? object)
  (is-a? object <turn-server>))



;; Convert an ALIST to a <turn-server> instance.
(define-method (alist->turn-server (alist <list>))
  (make <turn-server>
    #:password (assoc-ref alist "password")
    #:ttl      (assoc-ref alist "ttl")
    #:uris     (assoc-ref alist "uris")
    #:username (assoc-ref alist "username")))


