(define-module (deck core types matrix-event)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:export (<matrix-event>
            matrix-event?
            matrix-event-id
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



(define (matrix-event? object)
  (is-a? object <matrix-event>))



(define-method (alist->matrix-event (lst <list>))
  (make <matrix-event>
    #:id        (assoc-ref lst "event_id")
    #:room-id   (assoc-ref lst "room_id")
    #:sender-id (assoc-ref lst "sender")
    #:type      (assoc-ref lst "type")
    #:content   (assoc-ref lst "content")))

(define-method (matrix-event->alist (event <matrix-event>))
  `((event_id  . ,(matrix-event-id event))
    (room_id   . ,(matrix-event-room-id event))
    (sender    . ,(matrix-event-sender-id event))
    (type      . ,(matrix-event-type event))
    (content   . ,(matrix-event-content event))))


