;;;; eksd: Backend to the xxd-clone hex-dump program eksd.

;; Copyright © 2019–2024 Jaidyn Ann <jadedctrl@posteo.at>
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

(defpackage :eksd
  (:use :cl)
  (:export :stream-to-hex
           :file-to-hex
           :hex-to-char
           :hex-to-file
           :hex-to-integer
           :integer-to-hex
           :hex-to-stream
           :read-hex
           *ascii-text-table*
           *fancy-text-table*))

(in-package :eksd)


;;; Constants
;;; —————————————————————————————————————
(defparameter *ascii-text-table*
  '(("20" #\space)("21" #\!)("22" #\")("23" #\#)("24" #\$) ("25" #\%)
    ("26" #\&) ("27" #\') ("28" #\() ("29" #\)) ("2A" #\*) ("2B" #\+)
    ("2C" #\,) ("2D" #\-) ("2E" #\.) ("2F" #\/) ("30" #\0) ("31" #\1)
    ("32" #\2) ("33" #\3) ("34" #\4) ("35" #\5) ("36" #\6) ("37" #\7)
    ("38" #\8) ("39" #\9) ("3A" #\:) ("3B" #\;) ("3C" #\<) ("3D" #\=)
    ("3E" #\>) ("3F" #\?) ("40" #\@) ("41" #\A) ("42" #\B) ("43" #\C)
    ("44" #\D) ("45" #\E) ("46" #\F) ("47" #\G) ("48" #\H) ("49" #\I)
    ("4A" #\J) ("4B" #\K) ("4C" #\L) ("4D" #\M) ("4E" #\N) ("4F" #\O)
    ("50" #\P) ("51" #\Q) ("52" #\R) ("53" #\S) ("54" #\T) ("55" #\U)
    ("56" #\V) ("57" #\W) ("58" #\X) ("59" #\Y) ("5A" #\Z) ("5B" #\[)
    ("5C" #\\) ("5D" #\]) ("5E" #\^) ("5F" #\_) ("60" #\`) ("61" #\a)
    ("62" #\b) ("63" #\c) ("64" #\d) ("65" #\e) ("66" #\f) ("67" #\g)
    ("68" #\h) ("69" #\i) ("6A" #\j) ("6B" #\k) ("6C" #\l) ("6D" #\m)
    ("6E" #\n) ("6F" #\o) ("70" #\p) ("71" #\q) ("72" #\r) ("73" #\s)
    ("74" #\t) ("75" #\u) ("76" #\v) ("77" #\w) ("78" #\x) ("79" #\y)
    ("7A" #\z) ("7B" #\{) ("7C" #\|) ("7D" #\}) ("7E" #\~)))
(defparameter *fancy-text-bits*
  '(("00" #\×) ("07" #\🔔)("08" "⬅️") ("09" #\↣) ("0A" #\↵) ("0B" #\↧)
    ("0C" #\⇲) ("0D" #\↲) ("0E" #\⇪) ("0F" #\⇫) ("1B" #\↯) ("7F" #\⇐)))
(defparameter *fancy-text-table* (append *ascii-text-table* *fancy-text-bits*))



;;; X → Hex
;;; —————————————————————————————————————
;; stream → list-of-strings
(defun stream-to-hex (stream)
  "Return a stream's data as a list of hexadecimal strings."
  (loop :while   (listen stream)
        :collect (read-hex stream)))

;; pathname → list-of-strings
(defun file-to-hex (pathname)
  "Return a list of  a file's octets represented in hexadecimal strings."
  (with-open-file (fstream pathname
                           :direction :input :element-type '(unsigned-byte 8))
    (stream-to-hex fstream)))



;;; Hex → X
;;; —————————————————————————————————————
;; list-of-strings stream → nil
(defun hex-to-stream (hexes stream)
  "Write a list of bytes (in hex-string format) to a stream."
  (loop :for hex :in hexes
        :do (write-hex hex stream)))

;; list-of-strings pathname → nil
(defun hex-to-file (hexes pathname)
  "Write a list of bytes (in hex-string format) to a file."
  (with-open-file (fstream pathname
                           :direction :output :element-type '(unsigned-byte 8))
    (hex-to-stream hexes fstream)))



;; Text-table fun
;; —————————————————————————————————————
(defgeneric hex-to-char (hex/es &optional text-table) 
  (:documentation "Return a hexadecimal's respective character (as string)
                  according to the given text-table."))

;; string list → string
(defmethod hex-to-char ((hex string) &optional (text-table *ascii-text-table*))
  (or (cadr (assoc hex text-table :test #'equal)) #\.))

;; list-of-strings list → list-of-strings
(defmethod hex-to-char ((hexes list) &optional (text-table *ascii-text-table*))
  (mapcar (lambda (hex) (hex-to-char hex text-table)) hexes))


;; pathname list → list_of_strings
(defun file-to-char (pathname &optional (text-table *ascii-text-table*))
  "Print character representation of a file, as per the given character table."
  (hex-to-char (file-to-hex pathname) text-table))

;; character list → string
(defun char-hex (char &optional text-table)
  "Return a character's hex, given a text-table."
  (if (not text-table) 
    (integer-to-hex (char-code char))
    (cadr (assoc char (mapcar #'reverse text-table)))))

;; string list → list
(defun string-hex (string &optional text-table)
  "Given a string and text-table, return a list of its characters’ hex-codes."
  (loop :for char :across string
        :collect (char-hex char text-table)))



;;; Misc
;;; —————————————————————————————————————
;; stream → string
(defun read-hex (stream)
  "Read a byte from a stream as a hexcode."
  (integer-to-hex (read-byte stream)))

;; stream → string
(defun write-hex (hex stream)
  "Read a byte from a stream as a hexcode."
  (write-byte (hex-to-integer hex) stream))

;; number → string
(defun integer-to-hex (number)
  "Return the base-16 of a number."
  (format nil "~2,'0x" number))

;; string → number
(defun hex-to-integer (hex)
  "Convert hex to a base-10 integer."
  (parse-integer hex :radix 16))
