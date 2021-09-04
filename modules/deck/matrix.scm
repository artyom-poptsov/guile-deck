;;; matrix.scm -- Description of the <matrix> class.

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

;; This file contains a description of the main <matrix> class and the related
;; methods.
;;
;; The <matrix> class should be used as a starting point.


;;; Code:

(define-module (deck matrix)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (json)
  #:use-module (web uri)
  #:use-module (deck core common error)
  #:use-module (deck core common list)
  #:use-module (deck core net client)
  #:use-module (deck core types matrix-id)
  #:use-module (deck core mac)
  #:use-module (deck core session)
  #:export (<matrix>
            matrix-shared-secret
            matrix-server
            matrix-versions
            matrix-debug-mode?
            matrix-register
            matrix-login
            matrix-request-nonce))



(define-class <matrix> ()
  ;; <string>
  (shared-secret
   #:init-keyword #:shared-secret
   #:init-value   #f
   #:getter       matrix-shared-secret)

  ;; <client>
  (client
   #:init-value   #f
   #:getter       matrix-client
   #:setter       matrix-client-set!)

  ;; <boolean>
  (debug-mode?
   #:init-keyword #:debug-mode?
   #:init-value   #f
   #:getter       matrix-debug-mode?))

(define-method (initialize (matrix <matrix>) initargs)
  (next-method)
  (let ((home-server (constructor-argument #:home-server initargs)))

    (unless home-server
      (deck-error "No home server URI provided"))

    (matrix-client-set! matrix (make <client> #:server home-server))))



;; Gets the versions of the specification supported by the server.
(define-method (matrix-versions (matrix <matrix>))
  (client-get (matrix-client matrix) "/_matrix/client/versions"))

;; Get 'nonce' hash from a server.
(define-method (matrix-request-nonce (matrix <matrix>))
  (let ((result (client-get (matrix-client matrix)
                            "/_matrix/client/r0/admin/register")))
    (assoc-ref result "nonce")))



(define* (matrix-generate-mac matrix nonce user password
                              #:key
                              (admin? #f)
                              (user-type #f))
  (generate-mac (matrix-shared-secret matrix)
                (list nonce user password
                      (if admin?
                          "admin"
                          "notadmin"))))


;; Register a new user on the MATRIX server with the specified password.
(define* (matrix-register matrix user password
                          #:key
                          (admin?    #f)
                          (device-id #f)) ; not used yet
  (let* ((nonce  (matrix-request-nonce matrix))
         (mac    (matrix-generate-mac matrix
                                      nonce
                                      user
                                      password
                                      #:admin? admin?))
         (body   `((nonce    . ,nonce)
                   (username . ,user)
                   (password . ,password)
                   (mac      . ,mac)
                   (admin    . ,admin?))))
    (client-post (matrix-client matrix)
                 "/_matrix/client/r0/admin/register"
                 body)))


;; Try to authenticate on the Matrix server with the given credentials.
;; Return a new session instance.
(define-method (matrix-login (matrix   <matrix>)
                             (type     <string>)
                             (user     <string>)
                             (password <string>))
  (let ((response (client-post (matrix-client matrix)
                               "/_matrix/client/r0/login"
                               `((type     . ,type)
                                 (user     . ,user)
                                 (password . ,password)))))
    (unless response
      (deck-error "Could not authenticate"))

    (cond
     ((assoc-ref response "error")
      (deck-error (assoc-ref response "error")))
     (else
      (make <session>
        #:client  (matrix-client matrix)
        #:user-id (string->matrix-id (assoc-ref response "user_id"))
        #:token   (assoc-ref response "access_token"))))))
