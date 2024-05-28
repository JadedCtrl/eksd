;;;; eksd.unix.tests: Tests for eksd’s UNIX frontend package.

;; Copyright © 2024 Jaidyn Ann <jadedctrl@posteo.at>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

(defpackage :eksd/unix/tests
  (:use :cl :lisp-unit2)
  (:export :de-tail))

(in-package :eksd/unix/tests)

(define-test de-tail
    (:tags '(de-tail))
  (assert-equal (eksd/unix::de-tail '(1 2 3))
                '(1 2)))
