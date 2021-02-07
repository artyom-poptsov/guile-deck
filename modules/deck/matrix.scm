(define-module (deck matrix)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (json)
  #:use-module (web uri)
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
  (shared_secret
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
  (let ((home-server (and (memq #:home-server initargs)
                          (cadr (memq #:home-server initargs)))))

    (unless home-server
      (error "No home server URI provided"))

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
      (error "Could not authenticate"))
    (format #f "matrix-login: response: ~a~%" response)
    (make <session>
      #:user-id (string->matrix-id (assoc-ref response "user_id"))
      #:token   (assoc-ref response "access_token"))))

