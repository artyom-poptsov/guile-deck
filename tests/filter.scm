(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (deck core types filter))



(test-begin "filter")

(test-equal "event-filter->alist: All fields present"
  `(("limit"       . 10)
    ("not_senders" . ,(vector "@bob:matrix.org" "@malory:matrix.org"))
    ("not_types"   . ,(vector "some-excluded-type-1" "some-excluded-type-2"))
    ("senders"     . ,(vector "@alice:matrix.org"))
    ("types"       . ,(vector "some-type-1")))
  (let ((filter (make <event-filter>
                  #:limit   10
                  #:not-senders '("@bob:matrix.org" "@malory:matrix.org")
                  #:not-types   '("some-excluded-type-1" "some-excluded-type-2")
                  #:senders     '("@alice:matrix.org")
                  #:types       '("some-type-1"))))
    (event-filter->alist filter)))

(test-equal "event-filter->alist: Some fields absent"
  `(("limit"       . 10)
    ("senders"     . ,(vector "@alice:matrix.org"))
    ("types"       . ,(vector "some-type-1")))
  (let ((filter (make <event-filter>
                  #:limit   10
                  #:senders     '("@alice:matrix.org")
                  #:types       '("some-type-1"))))
    (event-filter->alist filter)))

(test-equal "state-filter->alist"
  `(("limit"        . 10)
    ("not_senders"  . ,(vector "@bob:matrix.org" "@malory:matrix.org"))
    ("not_types"    . ,(vector "some-excluded-type-1" "some-excluded-type-2"))
    ("senders"      . ,(vector "@alice:matrix.org"))
    ("types"        . ,(vector "some-type-1"))
    ("contains_url"              . #f)
    ("include_redundant_members" . #f)
    ("lazy_load_members"         . #f)
    ("not_rooms"    . ,(vector "!excluded-room:matrix.org"))
    ("rooms"        . ,(vector "!included-room:matrix.org")))
  (let ((filter (make <state-filter>
                  #:limit       10
                  #:not-senders  '("@bob:matrix.org" "@malory:matrix.org")
                  #:not-types    '("some-excluded-type-1" "some-excluded-type-2")
                  #:senders      '("@alice:matrix.org")
                  #:types        '("some-type-1")
                  #:contains-url               #f
                  #:include-redundant-members? #f
                  #:lazy-load-members?         #f
                  #:not-rooms    '("!excluded-room:matrix.org")
                  #:rooms        '("!included-room:matrix.org"))))
    (state-filter->alist filter)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "filter")

(exit exit-status)
