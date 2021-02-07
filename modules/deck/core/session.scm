(define-module (deck core session)
  #:use-module (oop goops)
  #:use-module (deck core types matrix-id)
  #:export (<session>
            session-user-id
            session-token))


(define-class <session> ()
  ;; <matrix-id>
  (user-id
   #:init-value   #f
   #:init-keyword #:user-id
   #:getter       session-user-id)

  ;; <string>
  (token
   #:init-value   #f
   #:init-keyword #:token
   #:getter       session-token))



(define-method (display (session <session>) (port <port>))
  (format port "#<session ~a ~a>"
          (matrix-id->string (session-user-id session))
          (number->string (object-address pipe) 16)))

(define-method (write (session <session>) (port <port>))
  (display session port))

(define-method (display (session <session>))
  (next-method)
  (display session (current-output-port)))

(define-method (write (session <session>))
  (next-method)
  (display session (current-output-port)))


