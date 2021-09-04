;;; room.scm -- Description of the <room> class.

;; Copyright (C) 2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; <room> class represents a Matrix room.


;;; Code:

(define-module (deck core room)
  #:use-module (oop goops)
  #:use-module (deck core common error)
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
            room-id/string
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
  (let ((id (construtor-argument #:id initargs)))

    (unless id
      (deck-error "No room Id was provided"))

    (cond
     ((string? id)
      (let ((matrix-id (string->matrix-id id)))
        (unless (equal? (matrix-id-type matrix-id) 'room)
          (deck-error "Wrong Matrix ID for room" matrix-id))
        (room-id-set! room matrix-id)))
     ((matrix-id? id)
      (unless (equal? (matrix-id-type id) 'room)
        (deck-error "Wrong Matrix ID for room" id)))
     (else
      (deck-error "Wrong Matrix ID for room" id)))))



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



(define-method (room-id/string (room <room>))
  (matrix-id->string (room-id room)))

(define-method (room-access-token (room <room>))
  (session-token (room-session room)))

;; Get the room access token in the form of an alist for a query.
(define-method (room-access-token/alist (room <room>))
  `(("access_token" . ,(room-access-token room))))

(define-method (room-has-access-token? (room <room>))
  (not (equal? (room-access-token room) #f)))

(define-method (assert-token (room <room>))
  (unless (room-has-access-token? room)
    (deck-error "Not logged in" room)))


;; Room-specific request methods.

(define* (get room resource #:key (query '()))
  (client-get (session-client (room-session room)) resource
              #:query (append query (room-access-token/alist room))))

(define* (put room resource body #:key (query '()))
  (client-put (session-client (room-session room)) resource body
              #:query (append query (room-access-token/alist room))))

(define* (post room resource body #:key (query '()))
  (client-post (session-client (room-session room)) resource body
               #:query (append query (room-access-token/alist room))))


;; This API returns a number of events in ROOM that happened just before and
;; after the specified EVENT.  This allows clients to get the context
;; surrounding an event.
(define* (room-event-context room event
                             #:key
                             (limit 10)
                             (filter #f))
  (assert-token room)
  (let* ((query  `(("limit" . ,(number->string limit))))
         (query  (if filter
                     (acons "filter" filter query)
                     query))
         (result (get room
                      (format #f "/_matrix/client/r0/rooms/~a/context/~a"
                              (room-id/string room)
                              (if (string? event)
                                  event
                                  (matrix-event-id/string event)))
                      #:query query)))
    result))



(define-generic room-invite)

(define-method (room-invite (room    <room>)
                            (user-id <matrix-id>))
  (assert-token room)
  (let ((result (post room
                      (format #f "/_matrix/client/r0/rooms/~a/invite"
                              (room-id/string room))
                      `(("user_id" . ,(matrix-id->string user-id))))))
    result))

(define-method (room-invite (room    <room>)
                            (user-id <string>))
  (room-invite room (string->matrix-id user-id)))



(define-method (room-join (room <room>))
  (assert-token room)
  (let ((result (client-post (session-client (room-session room))
                             (format #f "/_matrix/client/r0/rooms/~a/join"
                                     (room-id/string room))
                             '())))
    result))

(define-method (room-leave (room <room>))
  (assert-token room)
  (let ((result (post room
                      (format #f "/_matrix/client/r0/rooms/~a/leave"
                              (room-id/string room))
                      '())))
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
  (let ((result (post room
                      (format #f "/_matrix/client/r0/rooms/~a/ban"
                              (room-id/string room))
                      `(("reason"  . ,reason)
                        ("user_id" . ,(matrix-id->string user-id))))))
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
  (let ((result (post room
                      (format #f "/_matrix/client/r0/rooms/~a/unban"
                              (room-id/string room))
                      `(("user_id" . ,(matrix-id->string user-id))))))
    result))



;; This API updates the marker for the given receipt TYPE to the EVENT
;; specified.
(define* (room-receipt room event
                       #:key
                       (type "m.read")
                       (receipt '()))
  (assert-token room)
  (let ((result (post room
                      (format #f "/_matrix/client/r0/rooms/~a/receipt/~a/~a"
                              (room-id/string room)
                              type
                              (matrix-event-id/string event))
                      receipt)))
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
  (let* ((query  (if at
                     `(("at" . ,at))
                     '()))
         (query  (if membership
                     (acons "membership" membership query)
                     query))
         (query  (if not-membership
                     (acons "not_membership" not-membership query)
                     query))
         (result (get room
                      (format #f "/_matrix/client/r0/rooms/~a/messages"
                              (room-id/string room))
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
  (let* ((query  `(("limit" . ,(number->string limit))))
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
         (result (get room
                      (format #f "/_matrix/client/r0/rooms/~a/messages"
                              (room-id/string room))
                      #:query query)))
    result))

;; Get the state events for the current state of a ROOM.
(define-method (room-state (room <room>))
  (assert-token room)
  (let ((result (get room
                     (format #f "/_matrix/client/r0/rooms/~a/state"
                             (room-id/string room)))))
    (if result
        (map alist->matrix-event (vector->list result))
        result)))



(define-generic room-event)

(define-method (room-event (room <room>) (event-id <matrix-id>))
  (assert-token room)
  (let ((result (get room
                     (format #f "/_matrix/client/r0/rooms/~a/event/~a"
                             (room-id/string room)
                             (matrix-id->string event-id)))))
    (and result
         (alist->matrix-event result))))

(define-method (room-event (room <room>) (event-id <string>))
  (room-event room (string->matrix-id event-id)))



(define-generic room-send)

;; Send a message event to a ROOM. Message events allow access to historical
;; events and pagination, making them suited for "once-off" activity in a
;; room.
(define-method (room-send (room <room>)
                          (type <string>)
                          (body <list>)
                          (transaction-id <string>))
  (assert-token room)
  (let ((result (put room
                     (format #f "/_matrix/client/r0/rooms/~a/send/~a/~a"
                             (room-id/string room)
                             type
                             transaction-id)
                     body)))
    (if result
        (cond
         ((assoc-ref result "error")
          (deck-error (assoc-ref result "error") room type body transaction-id))
         (else
          (string->matrix-id (assoc-ref result "event_id"))))
        (deck-error "Could not send an event" room type body transaction-id))))

;; This version of 'room-send' uses the current time as a transaction ID.
(define-method (room-send (room <room>) (type <string>) (body <list>))
  (room-send room type body (number->string (current-time))))



