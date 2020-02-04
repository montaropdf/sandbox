;; (defun skip-then-read-char (s c)
;;   (if (char= c #\{)
;;       (format t "~a~%~a~%" c (read s t nil t))
;;       (format t "~a~%~a~%" c (read-preserving-whitespace s)))
;;   (read-char-no-hang s))

;; (let ((*readtable* (copy-readtable nil)))
;;   ;; (set-dispatch-macro-character #\# #\{ #'skip-then-read-char)
;;   ;; (set-dispatch-macro-character #\# #\} #'skip-then-read-char)
;;   (set-macro-character #\( #'skip-paren-then-read-char)
;;   (set-macro-character #\) #'skip-paren-then-read-char)
;;   (with-input-from-string (is "(123 x)
;; (456 y)")
;;     (format t "~S~%-------~%" (read is))
;;     (format t "~S~%~S~%-------~%" (read is) (read is))))

;; (defun skip-paren-then-read-char (s c)
;;   (if (char= c #\()
;;       (format t "After (: ~a~%~a~%" c (read s t nil t))
;;       (format t "Not After (: ~a~%~a~%" c (read-preserving-whitespace s)))
;;   (read-char-no-hang s))


;; (let ((*readtable* (copy-readtable nil))
;;       (cas "(123 x) (456 y)"))
;;   (set-macro-character #\( #'skip-paren-then-read-char)
;;   (set-macro-character #\) #'skip-paren-then-read-char)
;;   (with-input-from-string (is cas)
;;     (format t "--- Test 1 - Using readtable - Beginning read ~a ---~%~%" cas)
;;     (format t "~S~%-------~%" (read is))
;;     ;; (format t "~S~%~S~%-------~%" (read is) (read is))
;;     (format t "--- Test 1 - Using readtable - Ending read ~a ---~%~%" cas)
;;     ))

(defun display-read-data data
  (let ((data-as-string (format nil "~a" data))
        (data-length (length data-as-string)))
    (format t "--- Display ~a ---~%~%" data)
    ;; some-var is a cons cell, so checking what's in both cells.
    (format t "CAR: ~a~% CDR: ~a~%" (car data) (cdr data))
    ;; Loop over each element of some-var and display it, one element per line.
    (format t "Read result, one element/line:~%")
    (dolist (elt data)
      (format t "~a~%" elt))
    ;; Returns the content of some-var.
    (format t "some-var: ~a <> ~a ~a~%" data-as-string data data-length)
    ))


(let* ((cas "(123 x mix) (456 y)")
       (some-var (read-from-string cas))
       (some-var-as-string (format nil "~a" some-var))
       (some-var-length (length some-var-as-string))
       (some-other-var (read-from-string cas t nil :start some-var-length))
       )
  (progn
    (format t "--- Test 2 - Beginning read ~a ---~%~%" cas)
    ;; some-var is a cons cell, so checking what's in both cells.
    (format t "CAR: ~a~% CDR: ~a~%" (car some-var) (cdr some-var))
    ;; Loop over each element of some-var and display it, one element per line.
    (format t "Read result, one element/line:~%")
    (dolist (elt some-var)
      (format t "~a~%" elt))
    ;; Returns the content of some-var.
    (format t "some-var: ~a <> ~a ~a~%" some-var-as-string some-var some-var-length)
    (format t "some-other-var: ~a~%" some-other-var)
    (format t "--- Test 2 - Ending read ~a ---~%~%" cas)
    ;; This last line will generate an error at compile time.
    ;; set eof-error-p to nil (second argument) and the error will disappear.
    (read-from-string cas t nil :start 19)
    ))
;; INPUT to read: (hep ben 45 "galaxy")
;; INPUT to read (hello 1 4 (\"fcuk\" marre \"cannette\" 34 #\t) :x)
