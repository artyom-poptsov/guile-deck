;;; state.scm -- A description of <state> class.

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

;; The <state> class is used by 'room-sync' method to represent a fetched
;; Matrix state.


;;; Code:

(define-module (deck core types state)
  #:use-module (oop goops)
  #:use-module (deck core types matrix-id)
  #:export (<state>
            state?
            state-account-data
            state-device-lists
            state-device-one-time-keys-count
            state-next-batch
            state-presence
            state-presence-events
            state-presence-events-available?
            state-rooms
            state-rooms-invite
            state-rooms-invite-available?
            state-rooms-join
            state-rooms-join-available?
            state-rooms-leave
            state-rooms-leave-available?
            state-rooms-any-available?
            alist->state

            <room-update>
            list->room-update
            room-update-id
            room-update-content
            room-update-content:timeline
            timeline:limited?
            timeline:prev-batch
            timeline:events
            room-update-content:ephemeral
            room-update-content:account-data
            room-update-content:state))


;; See <https://matrix.org/docs/api/client-server/#!/Room32participation/sync>
(define-class <state> ()
  ;; The global private data created by this user.
  ;;
  ;; <list> of <matrix-event>
  (account-data
   #:init-keyword #:account-data
   #:getter       state-account-data)

  ;; Information on end-to-end device updates.
  (device-lists
   #:init-keyword #:device-lists
   #:getter       state-device-lists)

  ;; Information on end-to-end encryption keys.
  (device-one-time-keys-count
   #:init-keyword #:device-one-time-keys-count
   #:getter        state-device-one-time-keys-count)

  ;; REQUIRED. The batch token to supply in the since param of the next /sync
  ;; request.
  ;;
  ;; <string>
  (next-batch
   #:init-keyword  #:next-batch
   #:getter        state-next-batch)

  ;; The updates to the presence status of other users.
  ;;
  ;; <list> of <matrix-event>
  (presence
   #:init-keyword  #:presence
   #:getter        state-presence)

  ;; Updates to rooms.
  (rooms
   #:init-keyword  #:rooms
   #:getter        state-rooms)

   ;; Information on the send-to-device messages for the client device.
  (to-device
   #:init-keyword #:to-device
   #:getter       state-to-device))



;; This class describes an update to a room.
(define-class <room-update> ()
  ;; ID of the room.
  ;;
  ;; <matrix-id>
  (id
   #:init-keyword #:id
   #:getter       room-update-id)

  ;; The content of the update.
  ;;
  ;; <list>
  (content
   #:init-keyword #:content
   #:getter       room-update-content))

(define-method (display (update <room-update>) (port <port>))
  (format port "#<room-update ~a ~a>"
          (matrix-id->string (room-update-id update))
          (number->string (object-address pipe) 16)))

(define-method (write (update <room-update>) (port <port>))
  (display update port))

(define-method (display (update <room-update>))
  (next-method)
  (display update (current-output-port)))

(define-method (write (update <room-update>))
  (next-method)
  (display update (current-output-port)))

;; Convert a list LST from a "sync" response to a room update.
(define-method (list->room-update (lst <list>))
  (make <room-update>
    #:id      (string->matrix-id (car lst))
    #:content (cdr lst)))

(define-method (equal? (obj1 <room-update>) (obj2 <room-update>))
  (and (equal? (room-update-id obj1) (room-update-id obj2))
       (equal? (room-update-content obj1) (room-update-content obj2))))

(define-method (room-update-content:timeline (update <room-update>))
  (assoc-ref (room-update-content update) "timeline"))

(define-method (room-update-content:ephemeral (update <room-update>))
  (assoc-ref (room-update-content update) "ephemeral"))

(define-method (room-update-content:account-data (update <room-update>))
  (assoc-ref (room-update-content update) "account_data"))

(define-method (room-update-content:state (update <room-update>))
  (assoc-ref (room-update-content update) "state"))

(define-method (timeline:limited? (timeline <list>))
  (assoc-ref timeline "limited"))

(define-method (timeline:prev-batch (timeline <list>))
  (assoc-ref timeline "prev_batch"))

(define-method (timeline:events (timeline <list>))
  (assoc-ref timeline "events"))



(define-method (state? object)
  (is-a? <state> object))



(define-method (display (state <state>) (port <port>))
  (format port "#<state next-batch: ~a ~a>"
          (state-next-batch state)
          (number->string (object-address state) 16)))

(define-method (write (state <state>) (port <port>))
  (display state port))

(define-method (display (state <state>))
  (next-method)
  (display state (current-output-port)))

(define-method (write (state <state>))
  (next-method)
  (display state (current-output-port)))



(define-method (equal? (s1 <state>) (s2 <state>))
  (and (equal? (state-account-data               s1) (state-account-data s2))
       (equal? (state-device-lists               s1) (state-device-lists s2))
       (equal? (state-device-one-time-keys-count s1)
               (state-device-one-time-keys-count s2))
       (equal? (state-next-batch                 s1) (state-next-batch s2))
       (equal? (state-presence                   s1) (state-presence s2))
       (equal? (state-rooms                      s1) (state-rooms s2))
       (equal? (state-to-device                  s1) (state-to-device s2))))



(define-method (state-rooms-invite (state <state>))
  (assoc-ref (state-rooms state) "invite"))

(define-method (state-rooms-invite-available? (state <state>))
  (> (length (state-rooms-invite state)) 0))

(define-method (state-rooms-join (state <state>))
  (assoc-ref (state-rooms state) "join"))

(define-method (state-rooms-join-available? (state <state>))
  (> (length (state-rooms-join state)) 0))

(define-method (state-rooms-leave (state <state>))
  (assoc-ref (state-rooms state) "leave"))

(define-method (state-rooms-leave-available? (state <state>))
  (> (length (state-rooms-leave state)) 0))

(define-method (state-rooms-any-available? (state <state>))
  (or (state-rooms-invite-available? state)
      (state-rooms-join-available? state)
      (state-rooms-leave-available? state)))

(define-method (state-presence-events (state <state>))
  (assoc-ref (state-presence state) "events"))

(define-method (state-presence-events-available? (state <state>))
  (and (state-presence-events state)
       (> (vector-length (state-presence-events state)) 0)))



(define-method (alist->state (alist <list>))
  (let* ((rooms-updates (assoc-ref alist "rooms"))
         (invite        (assoc-ref rooms-updates "invite"))
         (join          (assoc-ref rooms-updates "join"))
         (leave         (assoc-ref rooms-updates "leave")))
    (make <state>
      #:account-data (assoc-ref alist "account_data")
      #:device-lists (assoc-ref alist "device_lists")
      #:device-one-time-keys-count (assoc-ref alist "device_one_time_keys_count")
      #:next-batch   (assoc-ref alist "next_batch")
      #:presence     (assoc-ref alist "presence")
      #:rooms        `(,(if (or (not invite) (null? invite))
                            (cons "invite" '())
                            (cons "invite" (map list->room-update invite)))
                       ,(if (or (not join) (null? join))
                            (cons "join" '())
                            (cons "join" (map list->room-update join)))
                       ,(if (or (not leave) (null? leave))
                            (cons "leave" '())
                            (cons "leave" (map list->room-update leave))))
      #:to-device    (assoc-ref alist "to_device"))))

