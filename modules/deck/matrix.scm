(define-module (deck matrix)
  #:use-module (oop goops)
  #:use-module (gcrypt mac)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (rnrs bytevectors)
  #:use-module (json)
  #:use-module (web uri)
  #:use-module (deck core net client)
  #:export (<matrix>
            matrix-shared-secret
            matrix-server
            matrix-versions
            matrix-debug-mode?
            matrix-register
            matrix-request-nonce
            generate-mac))



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



(define-method (matrix-versions (matrix <matrix>))
  (client-get (matrix-client matrix) "/_matrix/client/versions"))

(define-method (matrix-request-nonce (matrix <matrix>))
  (let ((result (client-get (matrix-client matrix)
                            "/_matrix/client/r0/admin/register")))
    (assoc-ref result "nonce")))


(define (bin->hex bv)
  "Convert a bytevector BV to a HEX string."
  (let ((hexits "0123456789abcdef"))
    (let loop ((idx 0)
               (out ""))
      (if (< idx (bytevector-length bv))
          (let ((elem (bytevector-u8-ref bv idx)))
            (loop (+ idx 1)
                  (string-append
                   out
                   (string (string-ref hexits (ash elem -4)))
                   (string (string-ref hexits (logand elem #x0F))))))
          out))))

(define-method (generate-mac (shared-secret <string>) (data <list>))
  (let ((bv (sign-data shared-secret (string-join data "\0")
                       #:algorithm (mac-algorithm hmac-sha1))))
    (bin->hex bv)))
    ;; (newline)
    ;; (fold (lambda (element prev)
    ;;         (display prev)
    ;;         (newline)
    ;;         (string-append prev (format #f "~:@(~2,'0X~)" element)))
    ;;       ""
    ;;       (bytevector->u8-list bv))))

(define* (matrix-generate-mac matrix nonce user password
                              #:key
                              (admin? #f)
                              (user-type #f))
  (generate-mac (matrix-shared-secret matrix)
                (list nonce user password
                      (if admin?
                          "admin"
                          "notadmin"))))


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
