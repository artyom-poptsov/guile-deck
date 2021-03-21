#!/usr/bin/guile-2.2 \
-L modules -e main -s
!#

;; Usage example:
;;   ./hello-bot.scm "matrix.org" "bot-login" '!room-id:matrix.org'

(use-modules (srfi srfi-1)
             (oop goops)
             (web uri)
             (deck matrix)
             (deck matrix-client)
             (deck core types matrix-id)
             (deck core types state)
             (deck core room)
             (deck core session))



(define (handle-update client room-id update)
  (format #t "handle-update: room-id: ~a; update: ~a~%" room-id update))

(define (handle-event client event-room-id room-id event)
  (when (equal? event-room-id room-id)
    (let* ((message-content (assoc-ref event "content"))
           (message-body    (assoc-ref message-content "body")))
      (when (or (string=? message-body "!hello")
                (string=? message-body "! hello"))
        (let ((room (matrix-client-room client room-id)))
          (format #t "room: ~a~%" room)
          (room-send room "m.room.message"
                     `(("body"    . "howdy!")
                       ("msgtype" . "m.text"))))))))


(define (main args)
  "Entry point"
  (let* ((hs           (list-ref args 1))
         (user         (list-ref args 2))
         (target-room  (list-ref args 3))
         (room-id      (string->matrix-id target-room))
         (password     (getpass "enter password: "))
         (matrix       (make <matrix>
                         #:use-https? #f
                         #:home-server (build-uri 'https #:host hs)))
         (session      (matrix-login matrix "m.login.password" user password))
         (client       (make <matrix-client>
                         #:session   session
                         #:on-update (lambda (client update)
                                       (handle-update client
                                                      room-id
                                                      update))
                         #:on-timeline-event `(("m.room.message"
                                                . ,(lambda (client event-room-id event)
                                                     (handle-event client
                                                                   event-room-id
                                                                   room-id
                                                                   event)))))))
    (session-join-room session target-room)
    (matrix-client-start! client)

    (while #t
      (sleep 1))))

