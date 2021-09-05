;;; matrix.scm -- Description of the <session> class.

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

;; <session> describes a Matrix client session. Usually this class is obtained
;; by means of 'matrix-login' call on <matrix> class.


;;; Code:

(define-module (deck core session)
  #:use-module (oop goops)
  #:use-module (deck core common error)
  #:use-module (deck core types matrix-id)
  #:use-module (deck core types matrix-content-uri)
  #:use-module (deck core types third-party-identifier)
  #:use-module (deck core types device)
  #:use-module (deck core types turn-server)
  #:use-module (deck core types state)
  #:use-module (deck core types filter)
  #:use-module (deck core room)
  #:use-module (deck core net client)
  #:export (<session>
            session?
            session-user-id
            session-3pid
            session-devices
            session-capabilities
            session-token
            session-token/alist
            session-client
            session-create-filter
            session-sync
            session-create-room
            session-join-room
            session-joined-rooms
            session-logout
            session-logout/all
            session-whoami
            session-avatar-uri
            session-voip-turn-server))


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

(define-method (session-3pid (session <session>))
  (let ((result (client-get (session-client session)
                            "/_matrix/client/r0/account/3pid"
                            #:query (session-token/alist session))))

    (unless result
      (deck-error "Could not make a request" session))

    (cond
     ((assoc-ref result "error")
      (deck-error (assoc-ref result "error")))
     (else
      (let ((threepids (assoc-ref result "threepids")))
        (format (current-error-port) "threepids: ~A~%" threepids)
        (map alist->third-party-identifier
             (vector->list threepids)))))))


;; Synchronise the client's state with the latest state on the server. Clients
;; use this API when they first log in to get an initial snapshot of the state
;; on the server, and then continue to call this API to get incremental deltas
;; to the state, and to receive new messages.
;;
;; See <https://matrix.org/docs/api/client-server/#!/Room32participation/sync>
(define* (session-sync session
                       #:key
                       (filter       #f)
                       (since        #f)
                       (full-state   #f)
                       (set-presense #f)
                       (timeout      #f))
  (let* ((query (session-token/alist session))
         (query (if filter
                    (acons "filter"
                           (if (filter? filter)
                               (filter->json-string filter)
                               filter)
                           query)
                    query))
         (query (if since
                    (acons "since" since query)
                    query))
         (query (if full-state
                    (acons "full_state" full-state query)
                    query))
         (query (if set-presense
                    (acons "set_presence" set-presense query)
                    query))
         (query (if timeout
                    (acons "timeout" timeout query)
                    query))
         (result (client-get (session-client session) "/_matrix/client/r0/sync"
                             #:query query)))
    (if result
        (cond
         ((assoc-ref result "error")
          (deck-error (assoc-ref result "error")))
         (else
          (alist->state result)))
        (deck-error "Could not make a request" session))))

;; Uploads a new filter definition to the homeserver. Returns a filter ID that
;; may be used in future requests to restrict which events are returned to the
;; client.
;;
;; See:
;;   <https://matrix.org/docs/api/client-server/#!/Room32participation/defineFilter>
(define-method (session-create-filter (session <session>) (filter <filter>))
  (let ((result (client-post (session-client session)
                             (format #f "/_matrix/client/r0/user/~a/filter"
                                     (session-user-id session))
                             (filter->alist filter)
                             #:query (session-token/alist session))))
    (unless result
      (deck-error "Could not make a request" session))

    (cond
     ((assoc-ref result "error")
      (deck-error (assoc-ref result "error")))
     (else
      (assoc-ref result "filter_id")))))



;; Gets information about all devices for the current user.
;;
;; Description:
;;   <https://matrix.org/docs/api/client-server/#!/Device32management/getDevices>
(define-method (session-devices (session <session>))
  (let ((result (client-get (session-client session)
                            "/_matrix/client/r0/devices"
                            #:query (session-token/alist session))))
    (if result
        (cond
         ((assoc-ref result "error")
          (deck-error (assoc-ref result "error") session))
         (else
          (map alist->device
               (vector->list (assoc-ref result "devices"))))))))



;; Gets information about the server's supported feature set and other
;; relevant capabilities.
;;
;; Description:
;;   <https://matrix.org/docs/api/client-server/#!/Capabilities/getCapabilities>
(define-method (session-capabilities (session <session>))
  (client-get (session-client session) "/_matrix/client/r0/capabilities"
              #:query (session-token/alist session)))



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

(define-method (session-join-room (session <session>)
                                  (room-id <string>))
  (session-join-room session (string->matrix-id room-id)))



(define-method (session-joined-rooms (session <session>))
  (let ((result (client-get (session-client session)
                            "/_matrix/client/r0/joined_rooms"
                            #:query (session-token/alist session))))
    (unless result
      (deck-error "Could not get the list of joined rooms"))
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
          (deck-error (assoc-ref result "error")))
         (else
          (string->matrix-id (assoc-ref result "user_id"))))
        (deck-error "Could not make a whoami request" session))))



(define-method (session-avatar-uri (session <session>))
  (let ((result (client-get (session-client session)
                            (format #f "/_matrix/client/r0/profile/~a/avatar_url"
                                    (session-user-id session))
                            #:query (session-token/alist session))))
    (if result
        (cond
         ((assoc-ref result "error")
          (deck-error (assoc-ref result "error")))
         (else
          (string->matrix-content-uri (assoc-ref result "avatar_url"))))
        (deck-error "Could not make a request" session))))



;; This API provides credentials for the client to use when initiating calls.
;; See <https://matrix.org/docs/api/client-server/#!/VOIP/getTurnServer>
(define-method (session-voip-turn-server (session <session>))
  (let ((result (client-get (session-client session)
                            "/_matrix/client/r0/voip/turnServer"
                            #:query (session-token/alist session))))
    (if result
        (cond
         ((assoc-ref result "error")
          (deck-error (assoc-ref result "error")))
         (else
          (alist->turn-server result)))
        (deck-error "Could not make a request" session))))


