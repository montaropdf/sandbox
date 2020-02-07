(defun display-read-data (data)
  (let* ((data-as-string (format nil "~a" data))
         (data-length (length data-as-string)))
    (format t "--- Display ~a ---~%~%" data)
    (format t "Type of data: ~a~%" (type-of data))
    ;; some-var is a cons cell, so checking what's in both cells.
    (format t "CAR: ~a~% CDR: ~a~%" (car data) (cdr data))
    ;; Loop over each element of some-var and display it, one element per line.
    (format t "Read result, one element/line:~%")
    (dolist (elt data)
      (format t "~a~%" elt))
    ;; Returns the content of some-var.
    (format t "some-var: ~a <> ~a ~a~%" data-as-string data data-length))
  )


(let ((cas "(123 x mix) (456 y)"))
  (format t "--- Test 2 - Beginning read ~a ---~%~%" cas)
  (loop with i = 0
        with some-var = nil
        do
           (progn
             (format t "i: ~a~%" i)
             (setf some-var (read-from-string cas t nil :start i))
             (setf i (+ i (length (format nil "~a" some-var))))
             (display-read-data some-var)
             ))
  )
