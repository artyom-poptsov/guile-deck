(define-module (deck core mac)
  #:use-module (oop goops)
  #:use-module (gcrypt mac)
  #:use-module (rnrs bytevectors)
  #:export (bin->hex
            generate-mac))


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
