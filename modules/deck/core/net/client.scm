;;; client.scm -- An HTTP client written for Matrix.

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

;; Auxiliary module for Guile-Deck internals. This module can be used to
;; connect to a Matrix home server.


;;; Code:

(define-module (deck core net client)
  #:use-module (srfi srfi-8)
  #:use-module (ice-9 iconv)
  #:use-module (oop goops)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (web response)
  #:use-module (json)
  #:export (<client>
            client?
            client-server-uri
            client-build-uri
            client-get
            client-put
            client-post
            uri-parameters->string
            client-set-debug!
            client-debug?))


(define-class <client> ()
  ;; <uri>
  (server-uri
   #:init-value   #f
   #:init-keyword #:server
   #:getter       client-server-uri)

  ;; <boolean>
  (debug?
   #:init-value   #f
   #:init-keyword #:debug?
   #:getter       client-debug?
   #:setter       client-set-debug!))

(define-method (client? object)
  (is-a? object <client>))



(define-method (display (client <client>) (port <port>))
  (format port "#<client ~a ~a>"
          (client-server-uri client)
          (number->string (object-address pipe) 16)))

(define-method (write (client <client>) (port <port>))
  (display client port))

(define-method (display (client <client>))
  (next-method)
  (display client (current-output-port)))

(define-method (write (client <client>))
  (next-method)
  (display client (current-output-port)))



(define-method (uri-parameters->string (params <list>))
  (if (null? params)
      ""
      (string-join (map (lambda (p)
                          (string-append (car p) "=" (cdr p)))
                        params)
                   "&")))

;; Build an URI based on the CLIENT parameters, a RESOURCE and a QUERY alist.
;; Returns the new URI.
(define-method (client-build-uri (client <client>) (resource <string>) (query <list>))
  (let ((server (client-server-uri client)))
    (build-uri (uri-scheme server)
               #:host   (uri-host server)
               #:port   (uri-port server)
               #:path   resource
               #:query  (uri-parameters->string query))))


(define* (client-get client
                     resource
                     #:key
                     (query '()))
  (let ((uri (client-build-uri client resource query)))
    (receive (response body)
        (http-get uri)
      (when (client-debug? client)
        (display response)
        (newline))
      (and (= (response-code response) 200)
           (json-string->scm (bytevector->string body "UTF-8"))))))

(define* (client-put client resource body
                      #:key
                      (query '()))
  (let ((uri       (client-build-uri client resource query))
        (json-body (scm->json-string body)))
    (receive (response response-body)
        (http-put uri
                  #:headers '((Content-Type . "application/json"))
                  #:port    (open-socket-for-uri uri)
                  #:body    json-body)
      (when (client-debug? client)
        (display response)
        (newline))
      (json-string->scm (bytevector->string response-body "UTF-8")))))

(define* (client-post client resource body
                      #:key
                      (query '()))
  (let ((uri       (client-build-uri client resource query))
        (json-body (scm->json-string body)))
    (receive (response response-body)
        (http-post uri
                   #:headers '((Content-Type . "application/json"))
                   #:port    (open-socket-for-uri uri)
                   #:body    json-body)
      (when (client-debug? client)
        (display response)
        (newline))
      (json-string->scm (bytevector->string response-body "UTF-8")))))
