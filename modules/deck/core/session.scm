(define-module (deck core session)
  #:use-module (oop goops)
  #:use-module (deck core types matrix-id)
  #:use-module (deck core room)
  #:use-module (deck core net client)
  #:export (<session>
            session?
            session-user-id
            session-token
            session-token/alist
            session-client
            session-sync
            session-create-room
            session-join-room
            session-joined-rooms
            session-logout
            session-logout/all
            session-whoami))


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



(define (session? object)
  (is-a? object <session>))

(define-method (session-token/alist (session <session>))
  `(("access_token" . ,(session-token session))))


(define-method (session-sync (session <session>))
  (let ((result (client-get (session-client session) "/_matrix/client/r0/sync"
                            #:query (session-token/alist session))))
    result))



(define-method (session-create-room (session <session>)
                                    (name    <string>))
  (let* ((body   `((room_alias_name . ,name)))
         (result (client-post (session-client session)
                             "/_matrix/client/r0/createRoom"
                             body
                             #:query (session-token/alist session))))
    (make <room>
      #:id    (assoc-ref result "room_id")
      #:alias (assoc-ref result "room_alias"))))

(define-method (session-join-room (session <session>)
                                  (room-id <matrix-id>))
  (let ((result (client-post (session-client session)
                             (string-append "/_matrix/client/r0/join/"
                                            (matrix-id->string room-id))
                             '()
                             #:query (session-token/alist session))))
    (make <room> #:session session #:id room-id)))

(define-method (session-joined-rooms (session <session>))
  (let ((result (client-get (session-client session)
                            "/_matrix/client/r0/joined_rooms"
                            #:query (session-token/alist session))))
    (unless result
      (error "Could not get the list of joined rooms"))
    (map (lambda (id) (make <room> #:session session #:id id))
         (vector->list (assoc-ref result "joined_rooms")))))


(define-method (session-logout (session <session>))
  (let ((result (client-post (session-client session)
                              "/_matrix/client/r0/logout"
                              '()
                              #:query (session-token/alist session))))
    result))

(define-method (session-logout/all (session <session>))
  (let ((result (client-post (session-client session)
                             "/_matrix/client/r0/logout/all"
                             '()
                             #:query (session-token/alist session))))
    result))


(define-method (session-whoami (session <session>))
  (let ((result (client-get (session-client session)
                            "/_matrix/client/r0/account/whoami"
                            #:query (session-token/alist session))))
    (if result
        (cond
         ((assoc-ref result "error")
          (error (assoc-ref result "error")))
         (else
          (string->matrix-id (assoc-ref result "user_id"))))
        (error "Could not make a whoami request" session))))


