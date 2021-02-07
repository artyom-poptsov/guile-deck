(define-module (deck core room)
  #:use-module (oop goops)
  #:use-module (deck core types matrix-id)
  #:use-module (deck core types matrix-content-uri)
  #:export (<room>
            room?
            room-alias
            room-id))



(define-class <room> ()
  ;; <matrix-id>
  (alias
   #:init-value   #f
   #:init-keyword #:alias
   #:getter       room-alias)

  ;; <matrix-id>
  (id
   #:init-value   #f
   #:init-keyword #:id
   #:getter       room-id))



(define-method (display (room <room>) (port <port>))
  (format port "#<room ~a ~a>"
          (matrix-id->string (room-id room))
          (number->string (object-address pipe) 16)))

(define-method (write (room <room>) (port <port>))
  (display room port))

(define-method (display (room <room>))
  (next-method)
  (display room (current-output-port)))

(define-method (write (room <room>))
  (next-method)
  (display room (current-output-port)))



(define-method (room? object)
  (is-a? object <room>))


