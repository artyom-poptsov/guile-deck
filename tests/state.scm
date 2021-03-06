(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (deck core types state))


(test-begin "state")

(test-equal "alist->state"
  (make <state>
    #:account-data               1
    #:device-lists               2
    #:device-one-time-keys-count 3
    #:next-batch                 4
    #:presense                   5
    #:rooms                      6
    #:to-device                  7)
  (alist->state '(("account_data"               . 1)
                  ("device_lists"               . 2)
                  ("device_one_time_keys_count" . 3)
                  ("next_batch"                 . 4)
                  ("presence"                   . 5)
                  ("rooms"                      . 6)
                  ("to_device"                  . 7))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "state")

(exit exit-status)


