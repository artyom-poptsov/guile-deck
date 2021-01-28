(define-module (deck core error)
  #:use-module (oop goops)
  #:export (deck-error))

(define-method (deck-error (message <string>))
  (throw 'deck-error message))
