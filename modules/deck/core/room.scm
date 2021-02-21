(define-module (deck core room)
  #:use-module (oop goops)
  #:use-module (deck core types matrix-event)
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
            room-access-token
            room-has-access-token?
            room-invite
            room-join
            room-leave
            room-ban
            room-unban
            room-members
            room-messages
            room-receipt
            room-state
            room-event
            room-event-context
            room-send))



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



(define-method (room-access-token (room <room>))
  (session-token (room-session room)))

;; Get the room access token in the form of an alist for a query.
(define-method (room-access-token/alist (room <room>))
  `(("access_token" . ,(room-access-token room))))

(define-method (room-has-access-token? (room <room>))
  (not (equal? (room-access-token room) #f)))

(define-method (assert-token (room <room>))
  (unless (room-has-access-token? room)
    (error "Not logged in")))


;; This API returns a number of events in ROOM that happened just before and
;; after the specified EVENT.  This allows clients to get the context
;; surrounding an event.
(define* (room-event-context room event
                             #:key
                             (limit 10)
                             (filter #f))
  (assert-token room)
  (let* ((query  (room-access-token/alist room))
         (query  (acons "limit" (number->string limit) query))
         (query  (if filter
                     (acons "filter" filter query)
                     query))
         (result (client-get (session-client (room-session room))
                             (format #f "/_matrix/client/r0/rooms/~a/context/~a"
                                     (matrix-id->string (room-id room))
                                     (if (string? event)
                                         event
                                         (matrix-id->string (matrix-event-id event))))
                             #:query query)))
    result))



(define-generic room-invite)

(define-method (room-invite (room    <room>)
                            (user-id <matrix-id>))
  (assert-token room)
  (let ((result (client-post (session-client (room-session room))
                             (format #f "/_matrix/client/r0/rooms/~a/invite"
                                     (matrix-id->string (room-id room)))
                             `(("user_id" . ,(matrix-id->string user-id)))
                             #:query (room-access-token/alist room))))
    result))

(define-method (room-invite (room    <room>)
                            (user-id <string>))
  (room-invite room (string->matrix-id user-id)))



(define-method (room-join (room <room>))
  (assert-token room)
  (let ((result (client-post (session-client (room-session room))
                             (format #f "/_matrix/client/r0/rooms/~a/join"
                                     (matrix-id->string (room-id room)))
                             '()
                             #:query (room-access-token/alist room))))
    result))

(define-method (room-leave (room <room>))
  (assert-token room)
  (let ((result (client-post (session-client (room-session room))
                             (format #f "/_matrix/client/r0/rooms/~a/leave"
                                     (matrix-id->string (room-id room)))
                             '()
                             #:query (room-access-token/alist room))))
    result))




(define-generic room-ban)

;; Ban a user with USER-ID in the ROOM. If the user is currently in the room,
;; also kick them. When a user is banned from a room, they may not join it or
;; be invited to it until they are unbanned.
;;
;; The caller must have the required power level in order to perform this
;; operation.
(define-method (room-ban (room <room>) (user-id <matrix-id>) (reason <string>))
  (assert-token room)
  (let ((result (client-post (session-client (room-session room))
                             (format #f "/_matrix/client/r0/rooms/~a/ban"
                                     (matrix-id->string (room-id room)))
                             `(("reason"  . ,reason)
                               ("user_id" . ,(matrix-id->string user-id)))
                             #:query (room-access-token/alist room))))
    result))

;; Shorter version of ban method without a reason.
(define-method (room-ban (room <room>) (user-id <matrix-id>))
  (room-ban room user-id ""))

;; Unban a user with USER-ID from the ROOM. This allows them to be invited to
;; the room, and join if they would otherwise be allowed to join according to
;; its join rules.
;;
;; The caller must have the required power level in order to perform this
;; operation.
(define-method (room-unban (room <room>) (user-id <matrix-id>))
  (assert-token room)
  (let ((result (client-post (session-client (room-session room))
                             (format #f "/_matrix/client/r0/rooms/~a/unban"
                                     (matrix-id->string (room-id room)))
                             `(("user_id" . ,(matrix-id->string user-id)))
                             #:query (room-access-token/alist room))))
    result))



;; This API updates the marker for the given receipt TYPE to the EVENT
;; specified.
(define* (room-receipt room event
                       #:key
                       (type "m.read")
                       (receipt '()))
  (assert-token room)
  (let ((result (client-post (session-client (room-session room))
                             (format #f "/_matrix/client/r0/rooms/~a/receipt/~a/~a"
                                     (matrix-id->string (room-id room))
                                     type
                                     (matrix-id->string (event-id event)))
                             receipt
                             #:query (room-access-token/alist room))))
    result))



;; Get the list of members for this room.
;;
;; Returns 3 values: an event list, "start"" and "end" tokens.
(define* (room-members room
                       #:key
                       (at             #f)
                       (membership     #f)
                       (not-membership #f))
  (assert-token room)
  (let* ((query  (room-access-token/alist room))
         (query  (if at
                     (acons "at" at query)
                     query))
         (query  (if membership
                     (acons "membership" membership query)
                     query))
         (query  (if not-membership
                     (acons "not_membership" not-membership query)
                     query))
         (result (client-get (session-client (room-session room))
                             (format #f "/_matrix/client/r0/rooms/~a/messages"
                                     (matrix-id->string (room-id room)))
                             #:query query)))
    (and result
         (let ((events (map alist->matrix-event
                            (vector->list (assoc-ref result "chunk"))))
               (start  (assoc-ref result "start"))
               (end    (assoc-ref result "end")))
           (values events start end)))))

(define* (room-messages room
                        #:key
                        (limit  10)
                        (from   #f)
                        (to     #f)
                        (filter #f))
  (assert-token room)
  (let* ((query  `(("access_token" . ,(session-token (room-session room)))
                   ("limit"        . ,(number->string limit))))
         (query  (if from
                     (acons "from" from query)
                     query))
         (query  (if to
                     (acons "to" to query)
                     query))
         (query  (if filter
                     (acons "filter" filter query)
                     query))
         (body   `())
         (result (client-get (session-client (room-session room))
                             (format #f "/_matrix/client/r0/rooms/~a/messages"
                                     (matrix-id->string (room-id room)))
                             #:query query)))
    result))

;; Get the state events for the current state of a ROOM.
(define-method (room-state (room <room>))
  (assert-token room)
  (let ((result (client-get (session-client (room-session room))
                            (format #f "/_matrix/client/r0/rooms/~a/state"
                                    (matrix-id->string (room-id room)))
                            #:query (room-access-token/alist room))))
    (if result
        (map alist->matrix-event (vector->list result))
        result)))



(define-generic room-event)

(define-method (room-event (room <room>) (event-id <matrix-id>))
  (assert-token room)
  (let ((result (client-get (session-client (room-session room))
                            (format #f "/_matrix/client/r0/rooms/~a/event/~a"
                                    (matrix-id->string (room-id room))
                                    (matrix-id->string event-id))
                            #:query (room-access-token/alist room))))
    (and result
         (alist->matrix-event result))))

(define-method (room-event (room <room>) (event-id <string>))
  (room-event room (string->matrix-id event-id)))



(define-generic room-send)

(define-method (room-send (room <room>)
                          (type <string>)
                          (body <list>)
                          (transaction-id <string>))
  (assert-token room)
  (let ((result (client-put (session-client (room-session room))
                            (format #f "/_matrix/client/r0/rooms/~a/send/~a/~a"
                                    (matrix-id->string (room-id room))
                                    type
                                    transaction-id)
                            body
                            #:query (room-access-token/alist room))))
    (if result
        (cond
         ((assoc-ref result "error")
          (error (assoc-ref result "error") room type body transaction-id))
         (else
          (string->matrix-id (assoc-ref result "event_id"))))
        (error "Could not send an event" room type body transaction-id))))

(define-method (room-send (room <room>) (type <string>) (body <list>))
  (room-send room type body ""))




