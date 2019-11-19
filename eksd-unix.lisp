;; This file is free software: you can redistribute it and/or modify
;; it under the terms of version 3 of the GNU General Public License
;; as published by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

(defpackage :eksd-unix
  (:use :cl :eksd)
  (:export :invoke
           *text-table*))

(in-package :eksd-unix)

;; —————————————————————————————————————

(opts:define-opts
  (:name :help
         :description "print this help text."
         :short #\h  :long "help")
  (:name :reverse
         :description "reverse operation: convert hexdump into binary."
         :short #\r  :long "reverse")
  (:name :upcase
         :description "print hexadecimal in uppercase."
         :short #\u  :long "upcase")
  (:name :width
         :description "octets per line; 16 as default."
         :short #\c  :long "cols"
         :arg-parser #'parse-integer
         :meta-var "COLS")
  (:name :seek
         :description "skip given amount of bytes in file."
         :short #\s  :long "seek"
         :arg-parser #'parse-integer
         :meta-var "OFFSET")
  (:name :group
         :description "octets per group; 2 as default."
         :short #\g  :long "groupsize"
         :arg-parser #'parse-integer
         :meta-var "OCTETS")
  (:name :ascii
         :description "use simple-ascii for previews; default is fun."
         :short #\a  :long "ascii")
  (:name :text-table
         :description "specify a text-table; semi-ascii as default."
         :short #\t  :long "table"
         :arg-parser #'probe-file
         :meta-var "TABLE"))


;; —————————————————————————————————————
;; MACROS

;; LIST SYMBOL FORM
(defmacro when-opt (opts option body)
  `(when (getf ,opts ,option)
     ,body))

;; LIST SYMBOL FORM FORM
(defmacro if-opt (opts option if-form &optional else-form)
  `(if (getf ,opts ,option)
     ,if-form ,else-form))


;; —————————————————————————————————————
;; MAIN - INVOCATION

;; NIL → NIL
(defun invoke ()
  "Actual invocation of the program. This is what you should set as :toplevel."
  (multiple-value-bind (opts free) (opts:get-opts)
    (when-opt opts :help (help))
    (choose-text-table opts)

    (let* ((input-stream (choose-input-stream free opts)))
      (when (not input-stream)
        (format *error-output* "eksd: No file provided and nothing piped.~%")
        (help 2))

      (choose-stream-position opts input-stream)
      (reverse-or-dump opts input-stream)

      (close input-stream))))


;; NUMBER STREAM → NIL
(defun help (&optional (exit-code 0) (stream *standard-output*))
  "Prints help message and dies."
  (unix-opts:describe :prefix "usage: eksd [-hr] [-t table-file] file"
                      :stream stream)
  (unix-opts:exit exit-code))

;; —————————————————

;; LIST STREAM → NIL
(defun reverse-or-dump (opts input-stream)
  "Determine if a hexdump or reversal's necessary— and execute it."
  (if-opt opts :reverse
    (reverse-stream input-stream)
    (apply #'print-stream (choose-pstream-args opts input-stream))))


;; —————————————————————————————————————
;; INPUT

;; LIST → STREAM
(defun choose-input-stream (free-args opts)
  "Select an input stream, between a file passed in free-args or stdin."
  (let* ((input-file   (car free-args))
         (input-file-p (ignore-errors (probe-file input-file)))
         (stdin-p      (listen *standard-input*)))
    (cond ((and (getf opts :reverse) input-file-p)
               (open input-file :direction :input :element-type 'character))
          (input-file-p (open-byte input-file))
          (stdin-p      *standard-input*))))

;; LIST → NIL
(defun choose-text-table (opts)
  "Choose the appropriate text-table— user-given or otherwise."
  (if-opt opts :text-table
          (setq *text-table* (parse-table-file (getf opts :text-table)))
          (if-opt opts :ascii
            (setq *text-table* eksd:*ascii-text-table*)
            (setq *text-table* eksd:*fancy-text-table*))))

;; LIST STREAM → NIL
(defun choose-stream-position (opts stream)
  "Choose the correct stream position— if seek arg used, etc."
  (when (not (eq stream *standard-input*))
    (if-opt opts :seek (file-position stream (getf opts :seek)))))

;; LIST STREAM → LIST
(defun choose-pstream-args (opts input-stream)
  "Take all options, and return the appropriate arguments to #'print-stream."
  (let ((args (list input-stream)))
    (when-opt opts :upcase (nconc args '(:upcase t)))
    (when-opt opts :width  (nconc args `(:width ,(getf opts :width))))
    (when-opt opts :group  (nconc args `(:group ,(getf opts :group))))
    args))

;; —————————————————

;; STREAM NUMBER NUMBER → LIST NUMBER
(defun get-line-hex (stream index width)
  "Return a line's worth of octets; and a new octet-index."
  (values
    (loop :while (listen stream) :for i :from 1 :to width
          :collect (eksd:read-hex stream) :do (incf index))
    index))


;; —————————————————————————————————————
;; OUTPUT

;; STREAM NUMBER NUMBER STREAM
(defun print-stream (stream &key (width 16) (group 2) (upcase nil) (out 't))
  "Print an entire stream in hex, xxd-style."
  (let ((index 0))
    (loop :while (listen stream)
          :do (setq index (print-line stream :out out :index index 
                                      :group group :width width
                                      :upcase upcase)))))


;; STREAM STREAM NUMBER NUMBER NUMBER → NUMBER
(defun print-line (stream &key (out 't) (index 0) (width 16) (group 2)
                               (upcase nil))
  "Print a given line of xxd-style output— index, bytes, preview and all.
  Returns the new index of the stream."
  (multiple-value-bind (hexes new-index) (get-line-hex stream index width)
    (print-index index out)
    (print-bytes (list-pad hexes width "  ") group upcase out)
    (print-preview hexes out)
    (format t "~%")
    new-index))

;; NUMBER STREAM → NIL
(defun print-index (index &optional (out 't))
  "Print the current index, padded to 8 char-length and in hexadecimal."
  (format out "~8,,,'0@A: " (string-downcase (eksd:integer-to-hex index))))

;; LIST_OF_STRINGS NUMBER STREAM → NIL
(defun print-bytes (hexes group-size upcase &optional (out 't))
  "Print the given list of bytes on a line in specified-sized groupings."
  (mapcar (lambda (group)
            (format out (if upcase "~{~@:(~a~)~} " "~{~(~a~)~} ") group))
          (pairs hexes group-size)))

;; LIST_OF_STRINGS STREAM → NIL
(defun print-preview (hexes &optional (out 't))
  "Print a given list of bytes' preview, as per ASCII table."
  (format out " ~{~A~}"
          (mapcar (lambda (hex) (hex-to-char hex *text-table*)) hexes)))


;; —————————————————————————————————————
;; REVERSAL

;; STREAM STREAM → NIL
(defun reverse-stream (stream &optional (out *standard-output*))
  "Take a stream of xxd-style/eksd-output hexcode and convert back into binary."
  (loop :while (listen stream)
        :do (mapcar (lambda (byte)
                      (write-byte (eksd:hex-to-integer byte) out))
                    (line-to-hexes (read-line stream)))))

;; STRING → LIST
(defun line-to-hexes (line)
  "Convert an xxd-style/eksd-output hexcode line into a list of hexcodes."
  (mapcar (lambda (pair) (format nil "~{~A~}" pair))
    (string-pairs
      (remove #\space (car (cl-strings:split
                             (left-clip-string line ": ")"  "))))))


;; —————————————————————————————————————
;; TEXT-TABLES

;; STRING → LIST
(defun parse-table-line (string)
  "Parse a text-table line into a pair of hex-code and preview character."
  (let ((chars (char-list string)))
    (list (format nil "~{~a~}" (list (car chars) (cadr chars)))
          (tail chars))))

;; PATHNAME → LIST
(defun parse-table-file (pathname)
  "Parse a text-table file (hexcode followed by preview character) into a list
  of lists '(hexcode character)."
  (with-open-file (istream pathname :direction :input :element-type 'character)
    (loop :while (listen istream)
          :collect (parse-table-line (read-line istream)))))


;; —————————————————————————————————————
;; MISC

;; LIST NUMBER VARYING → LIST
(defun list-pad (list target-length &optional (padding nil))
  "Pad a list out to length, by appending padding as necessary."
  (if (not (eq target-length (length list)))
    (list-pad (append list (list padding)) target-length padding)
    list))

;; LIST NUMBER → LIST
(defun pairs (list width &optional pairs)
  "Split a list into pairs (sublist) of a given width."
  (cond ((not list) pairs)
        ((or (eq width (length (tail pairs))) (not pairs))
         (pairs (cdr list) width (nconc pairs `((,(car list))))))
        ((not (eq width (length (tail pairs))))
         (pairs (cdr list) width
           (nconc (de-tail pairs) `(,(nconc (tail pairs) `(,(car list)))))))))

;; STRING CHARACTER → STRING
(defun left-clip-string (string &optional (seperator #\space))
  "Clip a string up to the first instance of the seperator."
  (reduce (lambda (a b) (format nil "~A~A~A" a seperator b))
          (cdr (cl-strings:split string seperator))))

;; STRING NUMBER → LIST
(defun string-pairs (string &optional (pair-length 2))
  "Return a list of characters from a string in pairs of given length."
  (pairs (char-list string) pair-length))

;; PATHNAME → STREAM
(defun open-byte (pathname)
  "Open an input file as a byte-stream."
  (open pathname :direction :input :element-type '(unsigned-byte 8)))

;; —————————————————

(defun char-list (string) (loop :for char :across string :collect char))
(defun de-tail (list) (reverse (cdr (reverse list))))
(defun tail  (list) (car (last list)))
