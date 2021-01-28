(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (deck core types matrix-id))

(test-begin "matrix-id")



(test-equal "matrix-type->char: user"
  #\@
  (matrix-type->char 'user))

(test-equal "matrix-type->char: room"
  #\!
  (matrix-type->char 'room))


(test-end "matrix-id")

(exit (= (test-runner-fail-count (test-runner-current)) 0))
