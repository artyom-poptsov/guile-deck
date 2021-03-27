;;; matrix-id.scm -- A description of <matrix-id> class.

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

;; This module contains the description of <matrix-id> class. This class is a
;; part of Guile-Deck core types.


;;; Code:

(define-module (deck core types matrix-id)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module (deck core common error)
  #:export (<matrix-id>
            matrix-id?
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



(define-method (display (matrix-id <matrix-id>) (port <port>))
  (format port "#<matrix-id ~a ~a>"
          (matrix-id->string matrix-id)
          (number->string (object-address matrix-id) 16)))

(define-method (write (matrix-id <matrix-id>) (port <port>))
  (display matrix-id port))

(define-method (display (matrix-id <matrix-id>))
  (next-method)
  (display matrix-id (current-output-port)))

(define-method (write (matrix-id <matrix-id>))
  (next-method)
  (display matrix-id (current-output-port)))

(define-method (equal? (id1 <matrix-id>) (id2 <matrix-id>))
  (and (equal? (matrix-id-type     id1) (matrix-id-type     id2))
       (equal? (matrix-id-server   id1) (matrix-id-server   id2))
       (equal? (matrix-id-identity id1) (matrix-id-identity id2))))


(define-method (matrix-type->char (identity <symbol>))
  (let ((result (find (lambda (e) (equal? (cdr e) identity)) %type-mapping)))
    (and result
         (car result))))

(define-method (char->matrix-type (ch <char>))
  (assoc-ref %type-mapping ch))


(define-method (matrix-id->string (id <matrix-id>))
  (format #f "~a~a~a"
          (matrix-type->char (matrix-id-type id))
          (matrix-id-identity id)
          (if (matrix-id-server id)
              (string-append ":" (matrix-id-server id))
              "")))


(define %identity-regexp         "^.([^:]+):.*")

;; https://matrix.org/docs/spec/rooms/v3
(define %event-identity-regexp-3 "^.([^:]+)$")

(define %server-regexp           "^.[^:]+:(.*)")

(define-method (string->matrix-id (string <string>))
  (let ((type (char->matrix-type (string-ref string 0))))
    (cond
     ((and (equal? type 'event)
           (string-match %event-identity-regexp-3 string))
      (make <matrix-id>
        #:type type
        #:identity (match:substring
                    (string-match %event-identity-regexp-3 string)
                    1)))
     (else
      (let ((identity (let ((m (string-match %identity-regexp string)))
                        (and m (match:substring m 1))))
            (server   (let ((m (string-match %server-regexp string)))
                        (and m (match:substring m 1)))))
        (if (and type identity server)
            (make <matrix-id>
              #:type       type
              #:identity   identity
              #:server     server)
            (deck-error "Wrong matrix ID" string)))))))



(define (matrix-id? object)
  (is-a? object <matrix-id>))
