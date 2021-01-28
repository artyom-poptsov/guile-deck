(define-module (deck core room)
  #:use-module (oop goops)
  #:use-module (deck core types matrix-id)
  #:use-module (deck core types matrix-content-uri)
  #:export (<room>
            room-alias
            room-id))



(define-class <room> ()
  ;; <matrix-id>
  (alias
   #:init-value   #f
   #:init-keyword #:alias
   #:getter       room-alias)

  ;; <matrix-id>
  (id
   #:init-value   #f
   #:init-keyword #:id
   #:getter       room-id))


