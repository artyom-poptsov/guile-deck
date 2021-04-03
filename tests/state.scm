(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (deck core types state)
             (deck core types matrix-id))


(test-begin "state")

(test-equal "alist->state"
  (make <state>
    #:account-data               1
    #:device-lists               2
    #:device-one-time-keys-count 3
    #:next-batch                 4
    #:presence                   5
    #:rooms                      '(("invite")
                                   ("join")
                                   ("leave"))
    #:to-device                  7)
  (alist->state '(("account_data"               . 1)
                  ("device_lists"               . 2)
                  ("device_one_time_keys_count" . 3)
                  ("next_batch"                 . 4)
                  ("presence"                   . 5)
                  ("rooms"                      . (("invite")
                                                   ("join")
                                                   ("leave")))
                  ("to_device"                  . 7))))

(test-assert "state-presence-events-available?"
  (let ((state (make <state> #:presence `(("events" . ,(vector "this" "is" "event"))))))
    (state-presence-events-available? state)))

(test-equal "list->room-update"
  (make <room-update>
    #:id      (string->matrix-id "!some-id:matrix.org")
    #:content '((org.matrix.msc2654.unread_count . 14626)
                (summary)
                (unread_notifications
                 (highlight_count . 0)
                 (notification_count . 0))
                (ephemeral (events . #(((content (user_ids . #()))
                                        (type . m.typing)))))
                (account_data (events . #()))
                (state (events . #()))
                (timeline (limited . #f)
                          (prev_batch . s1883439068)
                          (events . #()))))
  (list->room-update
   '("!some-id:matrix.org"
     (org.matrix.msc2654.unread_count . 14626)
     (summary)
     (unread_notifications
      (highlight_count . 0)
      (notification_count . 0))
     (ephemeral (events . #(((content (user_ids . #()))
                             (type . m.typing)))))
     (account_data (events . #()))
     (state (events . #()))
     (timeline (limited . #f)
               (prev_batch . s1883439068)
               (events . #())))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "state")

(exit exit-status)


