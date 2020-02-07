(defun display-read-data (data-stream eof-separator)
  (let* ((parse-result (read data-stream nil eof-separator))
         (data-as-string (format nil "~a" parse-result))
         ;; (data-length (length data-as-string))
         )
    (if (eq parse-result eof-separator)
        (progn
          (format t "--- End of string encountered: ~a ---~%" data-as-string)
          nil)
        (progn
          (format t "--- Display ~a ---~%~%" parse-result)
          (format t "Type of parse-result: ~a~%" (type-of parse-result))
          (when (typep parse-result 'cons)
            ;; parse-result is a cons cell, so checking what's in both cells.
            (format t "CAR: ~a~% CDR: ~a~%" (car parse-result) (cdr parse-result))
            ;; Loop over each element of parse-result and display it, one element per line.
            (format t "Read result, one element/line:~%")
            (dolist (elt parse-result)
              (format t "~a~%" elt)))
          ;; Display parse-result as a string and in its raw form.
          (format t "parse-result: ~a <> ~a~%" data-as-string parse-result)
          parse-result))))

(let ((input-string "(123 x mix) (456 y) merde (truth hurts) (bingo (1 3 4 65 23))"))
  (with-input-from-string (cas input-string)
    (format t "--- Test 3 - Beginning reading ~a ---~%~%" input-string)
    (do ((more-data t))
        ((not more-data))
      (setf more-data (display-read-data cas (list '$EOF$))))))
