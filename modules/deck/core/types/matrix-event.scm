;;; matrix-event.scm -- A description of <matrix-event> class.

;; Copyright (C) 2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This module contains the description of <matrix-event> class.


;;; Code:

(define-module (deck core types matrix-event)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module (deck core types matrix-id)
  #:export (<matrix-event>
            matrix-event?
            matrix-event-id
            matrix-event-id/string
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



(define-method (matrix-event-id/string (event <matrix-event>))
  (if (string? (matrix-event-id event))
      (matrix-event-id event)
      (matrix-id->string (matrix-event-id event))))



(define-method (display (event <matrix-event>) (port <port>))
  (format port "#<matrix-event ~a type: ~a ~a>"
          (matrix-event-id/string event)
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
  `(("event_id"  . ,(matrix-event-id/string event))
    ("user_id"   . ,(matrix-id->string (matrix-event-user-id event)))
    ("room_id"   . ,(matrix-id->string (matrix-event-room-id event)))
    ("sender"    . ,(matrix-id->string (matrix-event-sender-id event)))
    ("type"      . ,(matrix-event-type event))
    ("content"   . ,(matrix-event-content event))))


