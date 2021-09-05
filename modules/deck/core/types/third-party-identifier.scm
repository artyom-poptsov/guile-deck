;;; third-party-identifier.scm -- A description of <third-party-identifier> class.

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

;; See <https://matrix.org/docs/api/client-server/#!/User32data/getAccount3PIDs>


;;; Code:

(define-module (deck core types third-party-identifier)
  #:use-module (oop goops)
  #:export (<third-party-identifier>
            third-party-identifier-added-at
            third-party-identifier-validated-at
            third-party-identifier-address
            third-party-identifier-medium
            alist->third-party-identifier
            third-party-identifier->alist))



(define-class <third-party-identifier> ()
  ;; <number>
  (added-at
   #:init-keyword #:added-at
   #:init-value   #f
   #:getter       third-party-identifier-added-at)

  ;; <number>
  (validated-at
   #:init-keyword #:validated-at
   #:init-value   #f
   #:getter       third-party-identifier-validated-at)

  ;; <string>
  (address
   #:init-keyword #:address
   #:init-value   #f
   #:getter       third-party-identifier-address)

  ;; <string>
  (medium
   #:init-keyword #:medium
   #:init-value   #f
   #:getter       third-party-identifier-medium))



(define-method (%display (3pid <third-party-identifier>) (port <port>))
  (format port "#<third-party-identifier ~a: ~a ~a>"
          (third-party-identifier-medium 3pid)
          (third-party-identifier-address 3pid)
          (number->string (object-address 3pid) 16)))

(define-method (display (3pid <third-party-identifier>) (port <port>))
  (%display 3pid port))

(define-method (display (3pid <third-party-identifier>))
  (%display 3pid (current-output-port)))

(define-method (write (3pid <third-party-identifier>) (port <port>))
  (%display 3pid port))

(define-method (write (3pid <third-party-identifier>))
  (%display 3pid (current-output-port)))



(define-method (alist->third-party-identifier (alist <list>))
  (make <third-party-identifier>
    #:added-at     (assoc-ref alist "added_at")
    #:validated-at (assoc-ref alist "validated_at")
    #:address      (assoc-ref alist "address")
    #:medium       (assoc-ref alist "medium")))

(define-method (third-party-identifier->alist (3pid <third-party-identifier>))
  `(("added_at"     . ,(third-party-identifier-added-at 3pid))
    ("validated_at" . ,(third-party-identifier-validated-at 3pid))
    ("address"      . ,(third-party-identifier-address 3pid))
    ("medium"       . ,(third-party-identifier-medium 3pid))))

;;; third-party-identifier.scm ends here.
