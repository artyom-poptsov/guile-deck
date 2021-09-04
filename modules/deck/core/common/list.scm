;;; list.scm -- Common procedures for list handling.

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

;; This module contains common procedures for working with lists.


;;; Code:

(define-module (deck core common list)
  #:use-module (oop goops)
  #:export (constructor-argument
            cons-or-null
            make-sieved-list))

(define-syntax cons-or-null
  (syntax-rules ()
    ((_ key value)
     (if (not (equal? value 'undefined))
         (cons key value)
         '()))
    ((_ key value converter)
     (if (not (equal? value 'undefined))
         (cons key (converter value))
         '()))))

(define (make-sieved-list . elements)
  (delete '() elements))

(define (constructor-argument keyword initargs)
  (and (memq keyword initargs)
       (cadr (memq keyword initargs))))

;;; error.scm ends here.
