(define-module (deck core session)
  #:use-module (oop goops)
  #:use-module (deck core types matrix-id)
  #:use-module (deck core room)
  #:use-module (deck core net client)
  #:export (<session>
            session-user-id
            session-token
            session-create-room
            session-join-room))


(define-class <session> ()
  ;; <client>
  (client
   #:init-value   #f
   #:init-keyword #:client
   #:getter       session-client)

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



(define-method (session-create-room (session <session>)
                                    (name    <string>))
  (let* ((body   `((room_alias_name . ,name)))
         (query  `((access_token . ,(session-token session))))
         (result (client-post (session-client session)
                             "/_matrix/client/r0/createRoom"
                             body
                             #:query query)))
    (make <room>
      #:id    (string->matrix-id (assoc-ref result "room_id"))
      #:alias (assoc-ref result "room_alias"))))

(define-method (session-join-room (session <session>)
                                  (room-id <matrix-id>))
  (let* ((query  `(("access_token" . ,(session-token session))))
         (result (client-post (session-client session)
                              (string-append "/_matrix/client/r0/join/"
                                             (matrix-id->string room-id))
                              '()
                              #:query query)))
    (make <room> #:id room-id)))


