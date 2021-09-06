;;; presence.scm -- A description of <presence> class.

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

;; See:
;;   <https://matrix.org/docs/api/client-server/#!/Presence>


;;; Code:

(define-module (deck core types presence)
  #:use-module (oop goops)
  #:export (<presence>
            presence-currently-active?
            presence-last-active-ago
            presence-status
            presence-status-message))



(define-class <presence> ()
  ;; <boolean>
  (currently-active?
   #:init-keyword #:currently-active?
   #:init-value   #f
   #:getter       presence-currently-active?)

  ;; <number>
  (last-active-ago
   #:init-keyword #:last-active-ago
   #:init-value   #f
   #:getter       presence-last-active-ago)

  ;; <string>
  (status
   #:init-keyword #:status
   #:init-value   "offline"
   #:getter       presence-status)

  ;; <string>
  (status-message
   #:init-keyword #:status-message
   #:init-value   #f
   #:getter       presence-status-message))



(define-method (%display (presence <presence>) (port <port>))
  (format port "#<presence ~a~a ~a>"
          (presence-status presence)
          (if (presence-status-message presence)
              (format #f ": ~a" (presence-status-message presence))
              "")
          (number->string (object-address presence) 16)))

(define-method (display (presence <presence>) (port <port>))
  (%display presence port))

(define-method (display (presence <presence>))
  (%display presence (current-output-port)))

(define-method (write (presence <presence>) (port <port>))
  (%display presence port))

(define-method (write (presence <presence>))
  (%display presence (current-output-port)))



(define-method (equal? (p1 <presence>)
                       (p2 <presence>))
  (and (equal? (presence-currently-active? p1)
               (presence-currently-active? p2))
       (equal? (presence-last-active-ago   p1)
               (presence-last-active-ago   p2))
       (equal? (presence-status            p1)
               (presence-status            p2))
       (equal? (presence-status-message    p1)
               (presence-status-message    p2))))

;;; presence.scm ends here.
