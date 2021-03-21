;;; mac.scm -- Contains procedures for working with MAC.

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

;; Auxiliary module for Guile-Deck internals.


;;; Code:

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
