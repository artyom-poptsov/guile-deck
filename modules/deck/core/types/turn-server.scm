;;; turn-server.scm -- A description of <turn-server> class.

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

;; This module contains the description of <turn-server> class.


;;; Code:

(define-module (deck core types turn-server)
  #:use-module (oop goops)
  #:use-module (deck core types matrix-id)
  #:export (<turn-server>
            turn-server?
            turn-server-password
            turn-server-ttl
            turn-server-uris
            turn-server-username
            alist->turn-server))


;; See <https://matrix.org/docs/api/client-server/#!/VOIP/getTurnServer>
(define-class <turn-server> ()
  ;; <string>
  (password
   #:init-value   #f
   #:init-keyword #:password
   #:getter       turn-server-password)

  ;; <number>
  (ttl
   #:init-value   #f
   #:init-keyword #:ttl
   #:getter       turn-server-ttl)

  ;; <list>
  (uris
   #:init-value   '()
   #:init-keyword #:uris
   #:getter       turn-server-uris)

  ;; <string>
  (username
   #:init-value   #:f
   #:init-keyword #:username
   #:getter       turn-server-username))



(define-method (%display (turn-server <turn-server>) (port <port>))
  (format port "#<turn-server ~a>"
          (number->string (object-address turn-server) 16)))

(define-method (display (turn-server <turn-server>) (port <port>))
  (%display turn-server port))

(define-method (write (turn-server <turn-server>) (port <port>))
  (%display turn-server port))

(define-method (display (turn-server <turn-server>))
  (%display turn-server (current-output-port)))

(define-method (write (turn-server <turn-server>))
  (%display turn-server (current-output-port)))



(define-method (equal? (s1 <turn-server>) (s2 <turn-server>))
  (and (equal? (turn-server-password s1) (turn-server-password s2))
       (equal? (turn-server-ttl      s1) (turn-server-ttl      s2))
       (equal? (turn-server-uris     s1) (turn-server-uris     s2))
       (equal? (turn-server-username s1) (turn-server-username s2))))



(define (turn-server? object)
  (is-a? object <turn-server>))



;; Convert an ALIST to a <turn-server> instance.
(define-method (alist->turn-server (alist <list>))
  (make <turn-server>
    #:password (assoc-ref alist "password")
    #:ttl      (assoc-ref alist "ttl")
    #:uris     (assoc-ref alist "uris")
    #:username (assoc-ref alist "username")))


