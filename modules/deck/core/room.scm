(define-module (deck core room)
  #:use-module (oop goops)
  #:use-module (deck core types matrix-id)
  #:use-module (deck core types matrix-content-uri)
  #:use-module (deck core net client)
  #:use-module (deck core session)
  #:use-module (deck core room)
  #:export (<room>
            room?
            room-alias
            room-id
            room-session
            room-invite
            room-join))



(define-class <room> ()
  ;; <session>
  (session
   #:init-value   #f
   #:init-keyword #:session
   #:getter       room-session)

  ;; <matrix-id>
  (alias
   #:init-value   #f
   #:init-keyword #:alias
   #:getter       room-alias)

  ;; <matrix-id>
  (id
   #:init-value   #f
   #:init-keyword #:id
   #:getter       room-id
   #:setter       room-id-set!))



(define-method (initialize (room <room>) initargs)
  (next-method)
  (let ((id (and (memq #:id initargs)
                 (cadr (memq #:id initargs)))))

    (unless id
      (error "No room Id was provided"))

    (cond
     ((string? id)
      (let ((matrix-id (string->matrix-id id)))
        (unless (equal? (matrix-id-type matrix-id) 'room)
          (error "Wrong Matrix ID for room" matrix-id))
        (room-id-set! room matrix-id)))
     ((matrix-id? id)
      (unless (equal? (matrix-id-type id) 'room)
        (error "Wrong Matrix ID for room" id)))
     (else
      (error "Wrong Matrix ID for room" id)))))



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



(define-generic room-invite)

(define-method (room-invite (room    <room>)
                            (user-id <matrix-id>))
  (unless (session-token (room-session room))
    (error "Not logged it"))
  (let* ((query  `(("access_token" . ,(session-token (room-session room)))))
         (body   `(("user_id"      . ,(matrix-id->string user-id))))
         (result (client-post (session-client (room-session room))
                              (format #f "/_matrix/client/r0/rooms/~a/invite"
                                      (matrix-id->string (room-id room)))
                              body
                              #:query query)))
    result))

(define-method (room-invite (room    <room>)
                            (user-id <string>))
  (room-invite room (string->matrix-id user-id)))



(define-method (room-join (room <room>))
  (unless (session-token (room-session room))
    (error "Not logged in"))
  (let* ((query  `(("access_token" . ,(session-token (room-session room)))))
         (body   `())
         (result (client-post (session-client (room-session room))
                              (format #f "/_matrix/client/r0/rooms/~a/join"
                                      (matrix-id->string (room-id room)))
                              body
                              #:query query)))
    result))




