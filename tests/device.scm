(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (deck core types device))



(test-begin "device")

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


