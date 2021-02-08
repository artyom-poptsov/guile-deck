(define-module (deck core types matrix-event)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module (deck core types matrix-id)
  #:export (<matrix-event>
            matrix-event?
            matrix-event-id
            matrix-event-user-id
            matrix-event-room-id
            matrix-event-sender-id
            matrix-event-type
            matrix-event-content
            alist->matrix-event
            matrix-event->alist))



(define-class <matrix-event> ()
  ;; <matrix-id>
  (id
   #:init-value   #f
   #:init-keyword #:id
   #:getter       matrix-event-id)

  (user-id
   #:init-value   #f
   #:init-keyword #:user-id
   #:getter       matrix-event-user-id)

  ;; <matrix-id>
  (room-id
   #:init-value   #f
   #:init-keyword #:room-id
   #:getter       matrix-event-room-id)

  ;; <matrix-id>
  (sender-id
   #:init-value   #f
   #:init-keyword #:sender-id
   #:getter       matrix-event-sender-id)

  ;; <string>
  (type
   #:init-value   #f
   #:init-keyword #:type
   #:getter       matrix-event-type)

  ;; <list>
  (content
   #:init-value   '()
   #:init-keyword #:content
   #:getter       matrix-event-content))



(define-method (display (event <matrix-event>) (port <port>))
  (format port "#<matrix-event ~a type: ~a ~a>"
          (if (string? (matrix-event-id event))
              (matrix-event-id event)
              (matrix-id->string (matrix-event-id event)))
          (matrix-event-type event)
          (number->string (object-address pipe) 16)))

(define-method (write (event <matrix-event>) (port <port>))
  (display event port))

(define-method (display (event <matrix-event>))
  (next-method)
  (display event (current-output-port)))

(define-method (write (event <matrix-event>))
  (next-method)
  (display event (current-output-port)))



(define (matrix-event? object)
  (is-a? object <matrix-event>))



(define-method (alist->matrix-event (lst <list>))
  (display lst)
  (newline)
  (make <matrix-event>
    #:id        (catch
                  #t
                  (lambda ()
                    (string->matrix-id (assoc-ref lst "event_id")))
                  (lambda args
                    (assoc-ref lst "event_id")))
    #:user-id   (string->matrix-id (assoc-ref lst "user_id"))
    #:room-id   (string->matrix-id (assoc-ref lst "room_id"))
    #:sender-id (string->matrix-id (assoc-ref lst "sender"))
    #:type      (assoc-ref lst "type")
    #:content   (assoc-ref lst "content")))

(define-method (matrix-event->alist (event <matrix-event>))
  `((event_id  . ,(matrix-event-id event))
    (room_id   . ,(matrix-event-room-id event))
    (sender    . ,(matrix-event-sender-id event))
    (type      . ,(matrix-event-type event))
    (content   . ,(matrix-event-content event))))


