;;; Testing the READ function with a recursive function to parse deeper lisp objects.
;;;
;;; This exercise is a preparation for working on the reader that will
;;; allow managing the file format described at
;;; https://github.com/montaropdf/reve-workshop/blob/dev/docs/notes/lispdb.org.
;;;
;;; The goal of this exercise is to extend the reader in reader3.lisp
;;; to be able to deal with SEXP in SEXP. This is better done using a
;;; recursive function. Each call to the recursive function will read
;;; an SEXP and display various information about the object
;;; discovered in the SEXP.
;;;
;;; This code is not meant to be clean, robust, secure or efficient,
;;; My only concern is for the program to be able to read and analyse
;;; a lisp code of an arbitrary depth.
;;;
;;; Questions to the reader of this file (that is you):
;;; - Given the description above, is the usage of READ, appropriate?
;;; - Is there a better solution than the REPEAT-CHAR function.
;;; - What to use in place of FORMAT to display text on the screen
;;;   (logging packages or functions).
;;; - *DEPTH* is used to define the indentation level of the text
;;;   displayed. Is it possible to avoid using the global variable,
;;;   without adding a parameter to all functions to transmit that
;;;   information? (a static variable in the appropriate functions?)
;;;
;;;
;;; Some examples of formatting control from pjb on IRC #clschool channel
;;; <pjb> montaropdf: you can use V in the arguments of a format
;;; specifier to get it from a format argument.  [15:46]
;;; <pjb> specbot clhs ~I
;;; <pjb> sorry.  [15:47]
;;; <pjb> Yes, ~I would work only with pretty-printing.
;;; <pjb> montaropdf: for example: (format t ">~VT~A<" 4 33) #| >
;;; 33< --> nil |#  [15:48]
;;; <pjb> montaropdf: note: this works only for arguments, not for
;;; the @ and : flags.
;;; <pjb> (format nil "~V,VD" 8 #\@ 42) #| --> "@@@@@@42" |#
;;; [15:49]
;;; -------------------------------------------------------------------------------

(defparameter *depth* 0)

(defun repeat-char (char-to-replicate number-of-replication)
  "Return a string made of NUMBER-OF-REPLICATION times the character CHAR-TO-REPLICATE."
  (make-string  number-of-replication :initial-element char-to-replicate))

(defun heading (text)
  "Return TEXT enclosed by the right amount of - characters."
  (format t "~a ~a ~a~%" (repeat-char #\- *depth*) text (repeat-char #\- *depth*)))

(defun indent (text)
  "Return TEXT indented correctly."
  (let ((format-string (format nil "~~~DT ~~a~~%" *depth*)))
    (format t format-string text)))

(defun display-read-data (data-stream eof-separator)
  "Read the DATA-STREAM, assuming it contains lisp expression and display various information about it."
  (let* ((parse-result (read data-stream nil eof-separator))
         (data-as-string (format nil "~a" parse-result)))
    (if (eq parse-result eof-separator)
        (progn
          (heading (format nil "End of string encountered: ~a" data-as-string))
          nil)
        (progn
          (heading (format nil "Display ~a" parse-result))
          (indent (format nil "Type of parse-result: ~a" (type-of parse-result)))
          ;; Display parse-result as a string and in its raw form.
          (indent (format nil "parse-result: ~a <> ~a" data-as-string parse-result))
          (if (typep parse-result 'cons)
              ;; parse-result is a cons cell, so checking what's in both cells.
              (progn
                (indent (format nil "CAR: ~a" (car parse-result)))
                (indent (format nil "CDR: ~a" (cdr parse-result)))
                ;; Loop over each element of parse-result and display it, one element per line.
                (indent "Read result, one element/line:")
                (dolist (elt parse-result)
                  (indent elt)
                  (with-input-from-string (cas (format nil "~a" elt))
                    (incf *depth*)
                    (display-read-data cas eof-separator))))
              (progn
                (heading (format nil "Simple data type encountered: ~a" parse-result))
                (unless (eq *depth* 1) (decf *depth*))))
          parse-result))))

(let ((input-string "(123 x mix) (456 y) merde (truth hurts) (bingo (1 3 4 65 23))"))
  (with-input-from-string (cas input-string)
    (setf *depth* 10)
    (heading (format nil "Test 4 - Begin reading ~a" input-string))
    (setf *depth* 1)
    (do ((more-data t))
        ((not more-data))
      (setf more-data (display-read-data cas (list '$EOF$))))))
