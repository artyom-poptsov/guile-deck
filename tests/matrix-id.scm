(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (deck core types matrix-id))

(test-begin "matrix-id")



(test-equal "matrix-type->char: user"
  #\@
  (matrix-type->char 'user))

(test-equal "matrix-type->char: room"
  #\!
  (matrix-type->char 'room))

(test-equal "matrix-type->char: alias"
  #\#
  (matrix-type->char 'alias))

(test-equal "matrix-type->char: event"
  #\$
  (matrix-type->char 'event))

(test-equal "char->matrix-type: user"
  'user
  (char->matrix-type #\@))

(test-equal "char->matrix-type: user"
  'room
  (char->matrix-type #\!))

(test-equal "char->matrix-type: user"
  'alias
  (char->matrix-type #\#))

(test-equal "char->matrix-type: user"
  'event
  (char->matrix-type #\$))



(test-assert "matrix-id?"
  (and (matrix-id? (make <matrix-id>))
       (not (matrix-id? "not ID"))))



(test-equal "matrix-id->string: room"
  "!room-id:example.org"
  (matrix-id->string (make <matrix-id>
                       #:type     'room
                       #:identity "room-id"
                       #:server   "example.org")))



(test-equal "string->matrix-id: room type"
  'room
  (matrix-id-type (string->matrix-id "!room-id:example.org")))

(test-equal "string->matrix-id: room identity"
  "room-id"
  (matrix-id-identity (string->matrix-id "!room-id:example.org")))

(test-equal "string->matrix-id: room server"
  "example.org"
  (matrix-id-server (string->matrix-id "!room-id:example.org")))

(test-assert "string->matrix-id: room"
  (let ((id (string->matrix-id "!room-id:example.org")))
    (and (equal? (matrix-id-type id)     'room)
         (equal? (matrix-id-identity id) "room-id")
         (equal? (matrix-id-server id)   "example.org"))))


(test-equal "string->matrix-id: event identity"
  "abc123"
  (matrix-id-identity
   (string->matrix-id "$abc123:matrix.org")))

(test-equal "string->matrix-id: event identity (v3)"
  "tkNd9FVwPyAlcKYvLfgAbFseQ5pAJRKlWB_clzvloqY"
  (matrix-id-identity
   (string->matrix-id "$tkNd9FVwPyAlcKYvLfgAbFseQ5pAJRKlWB_clzvloqY")))


(test-end "matrix-id")

(exit (= (test-runner-fail-count (test-runner-current)) 0))
