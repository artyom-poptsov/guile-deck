;;; matrix-content-uri.scm -- A description of <matrix-content-uri>> class.

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

;; This module contains the description of <matrix-content-uri> class.


;;; Code:

(define-module (deck core types matrix-content-uri)
  #:use-module (oop goops)
  #:use-module (ice-9 regex)
  #:export (<matrix-content-uri>
            matrix-content-uri?
            matrix-content-uri-server
            matrix-content-uri-protocol
            matrix-content-uri-media-id
            string->matrix-content-uri
            matrix-content-uri->string))


(define-class <matrix-content-uri> ()
  ;; <string>
  (server
   #:init-value   #f
   #:init-keyword #:server
   #:getter       matrix-content-uri-server)

  ;; <string>
  (protocol
   #:init-value   "mxc"
   #:init-keyword #:protocol
   #:getter       matrix-content-uri-protocol)

  ;; <string>
  (media-id
   #:init-value   #f
   #:init-keyword #:media-id
   #:getter       matrix-content-uri-media-id))



(define-method (display (uri <matrix-content-uri>) (port <port>))
  (format port "#<matrix-content-uri ~a://~a/~a ~a>"
          (matrix-content-uri-protocol uri)
          (matrix-content-uri-server uri)
          (matrix-content-uri-media-id uri)
          (number->string (object-address pipe) 16)))

(define-method (write (uri <matrix-content-uri>) (port <port>))
  (display uri port))

(define-method (display (uri <matrix-content-uri>))
  (next-method)
  (display uri (current-output-port)))

(define-method (write (uri <matrix-content-uri>))
  (next-method)
  (display uri (current-output-port)))



(define-method (matrix-content-uri? object)
  (is-a? object <matrix-content-uri>))



(define-method (matrix-content-uri-protocol (string <string>))
  (let ((m (string-match "([a-z]+)://*" string)))
    (and m
         (match:substring m 1))))

(define-method (matrix-content-uri-server (string <string>))
  (let ((m (string-match "[a-z]+://([^/]+)/*" string)))
    (and m
         (match:substring m 1))))

(define-method (matrix-content-uri-media-id (string <string>))
  (let ((m (string-match "[a-z]+://[^/]+/(.*)" string)))
    (and m
         (match:substring m 1))))

  
(define-method (string->matrix-content-uri (string <string>))
  (make <matrix-content-uri>
    #:server   (matrix-content-uri-server string)
    #:protocol (matrix-content-uri-protocol string)
    #:media-id (matrix-content-uri-media-id string)))

(define-method (matrix-content-uri->string (uri <matrix-content-uri>))
  (format #f "~a://~a/~a"
          (matrix-content-uri-protocol uri)
          (matrix-content-uri-server uri)
          (matrix-content-uri-media-id)))

  
