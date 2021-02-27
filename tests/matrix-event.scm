(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (deck core types matrix-event)
             (deck core types matrix-id))


(test-begin "matrix-event")

(test-assert "matrix-event?"
  (and (matrix-event? (make <matrix-event>))
       (not (matrix-event? "not an event"))))



(test-assert "alist->matrix-event->alist"
  (let* ((lst '(("event_id" . "$e")
                ("user_id"  . "@u1:example.com")
                ("room_id"  . "!r:example.com")
                ("sender"   . "@u2:example.com")
                ("type"     . "m.test.type")
                ("content"  . '())))
         (e (alist->matrix-event lst)))
    (equal? lst (matrix-event->alist e))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "matrix-event")

(exit exit-status)
