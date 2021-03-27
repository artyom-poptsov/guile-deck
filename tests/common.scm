(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 regex)
             (oop goops)
             (deck core common list))



(test-begin "common")

(test-equal "cons-or-null: key, value"
  '(key . value)
  (cons-or-null 'key 'value))

(test-equal "cons-or-null: key, value (undefined)"
  '()
  (cons-or-null 'key 'undefined))

(test-equal "cons-or-null: key, value, converter"
  '(key . "123")
  (cons-or-null 'key 123 number->string))

(test-equal "cons-or-null: key, value (undefined), converter"
  '()
  (cons-or-null 'key 'undefined number->string))



(test-equal "make-sieved-list"
  '(a b c)
  (make-sieved-list 'a '() 'b '() 'c))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "common")

(exit exit-status)
