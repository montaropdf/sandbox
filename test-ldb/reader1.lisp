(let* ((cas "(x)")
       (some-var (read-from-string cas))
       (some-var-as-string (format nil "~a" some-var))
       (some-var-length (length some-var-as-string))
       ;; This last line will generate an error at compile time.
       ;; set eof-error-p to nil (second argument) and the error will disappear.
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
    (read-from-string cas t nil :start 18)
    ))
