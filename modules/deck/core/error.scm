(define-module (deck core error)
  #:export (deck-error))

(define-method (deck-error (message <string>))
  (throw 'deck-error message))
