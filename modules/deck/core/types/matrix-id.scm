(define-module (deck core types matrix-id)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:export (<matrix-id>
            matrix-id-type
            matrix-id-identity
            matrix-id-server
            matrix-type->char
            char->matrix-type
            matrix-id->string
            string->matrix-id))


(define %type-mapping
  '((#\@ . user)
    (#\! . room)
    (#\# . alias)
    (#\$ . event)))


(define-class <matrix-id> ()
  ;; <symbol>
  (type
   #:init-value   #f
   #:init-keyword #:type
   #:getter       matrix-id-type)

  ;; <string>
  (server
   #:init-value   #f
   #:init-keyword #:server
   #:getter       matrix-id-server)

  ;; <string>
  (identity
   #:init-value   #:f
   #:init-keyword #:identity
   #:getter       matrix-id-identity))


(define-method (matrix-type->char (identity <symbol>))
  (let ((result (find (lambda (e) (equal? (cdr e) identity)) %type-mapping)))
    (and result
         (car result))))

(define-method (char->matrix-type (ch <char>))
  (assoc-ref %type-mapping ch))


(define-method (matrix-id->string (id <matrix-id>))
  (format #f "~a~a:~a"
          (matrix-type->char (matrix-id-type id))
          (matrix-id-identity id)
          (matrix-id-server id)))

(define-method (string->matrix-id (string <string>))
  (let ((type     (char->matrix-type (string-ref string 0)))
        (identity (let ((m (string-match ".?([^:]+):.*" string)))
                    (and m
                         (match:substring m 1))))
        (server   (let ((m (string-match ".?[^:]+:(.*)" string)))
                    (and m
                         (match:substring m 1)))))
    (if (and type identity server)
        (make <matrix-id>
          #:type       type
          #:identity   identity
          #:server     server)
        (error "Wrong matrix ID"))))


