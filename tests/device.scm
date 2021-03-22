(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 regex)
             (oop goops)
             (deck core types device))



(test-begin "device")

(test-assert "device?"
  (and (device? (make <device>
                  #:id                  "id"
                  #:display-name        "name"
                  #:last-seen-ip        "ip"
                  #:last-seen-timestamp "timestamp"))
       (not (device? "not a device"))))

(test-assert "display"
  (let ((output (with-output-to-string
                  (lambda ()
                    (display (make <device>
                               #:id                  "id"
                               #:display-name        "name"
                               #:last-seen-ip        "ip"
                               #:last-seen-timestamp "timestamp"))))))
    (string-match "#<device id [0-9a-z]+>" output)))

(test-equal "equal?"
  (make <device>
    #:id                  "id"
    #:display-name        "name"
    #:last-seen-ip        "ip"
    #:last-seen-timestamp "timestamp")
  (make <device>
    #:id                  "id"
    #:display-name        "name"
    #:last-seen-ip        "ip"
    #:last-seen-timestamp "timestamp"))

(test-equal "alist->device"
  (make <device>
    #:id "id"
    #:display-name "name"
    #:last-seen-ip "ip"
    #:last-seen-timestamp "timestamp")
  (alist->device '(("device_id"    . "id")
                   ("display_name" . "name")
                   ("last_seen_ip" . "ip")
                   ("last_seen_ts" . "timestamp"))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "device")

(exit exit-status)


