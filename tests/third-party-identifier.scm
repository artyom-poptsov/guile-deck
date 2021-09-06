(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 regex)
             (oop goops)
             (deck core types third-party-identifier))


(define %test-name "third-party-identifier")

(test-begin %test-name)


(test-equal "alist->third-party-identifier"
  (make <third-party-identifier>
    #:added-at     1234567890
    #:validated-at 987654321
    #:address      "avp@example.org"
    #:medium       "email")
  (alist->third-party-identifier
   '(("added_at"     . 1234567890)
     ("validated_at" . 987654321)
     ("address"      . "avp@example.org")
     ("medium"       . "email"))))

(test-equal "third-party-identifier->alist"
    '(("added_at"     . 1234567890)
      ("validated_at" . 987654321)
      ("address"      . "avp@example.org")
      ("medium"       . "email"))
    (third-party-identifier->alist
     (make <third-party-identifier>
       #:added-at     1234567890
       #:validated-at 987654321
       #:address      "avp@example.org"
       #:medium       "email")))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
