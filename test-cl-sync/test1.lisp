(ql:quickload "cl-async")
(ql:quickload "babel")

(defmacro shutdown-message ()
  (format nil "Request of shutdown received.~%You will be disconnected.~%Goodbye!~%"))

(defmacro disconnect-message ()
  (format nil "It was a pleasure to serve you, sir.~%See you soon.~%Goodbye!~%"))

(defmacro welcome-message ()
  (format nil "Welcome sir,~%How may I help you?~%"))

(defun test-server ()
  (progn
    (format t "Initializing Test Server...~%")
    (cl-async:tcp-server
     "127.0.0.1" 6666
     (lambda (sock data)
       (let ((data-as-string (uiop:stripln
                              (babel:octets-to-string data))))
         
         (format t "Data received as string: ~a~%"
                 data-as-string)
         (format t "Raw data received: ~a~%" data)
         (as:write-socket-data sock data)
         
         (cond ((string= data-as-string "(shutdown)")
                (progn
                  (format t
                          "Request to shutdown the server received~%")
                  (as:write-socket-data
                   sock (shutdown-message)
                   :write-cb (lambda (socket)
                               (progn
                                 (as:close-socket socket)
                                 (as:exit-event-loop))))))
               ((string= data-as-string "(disconnect)")
                (progn
                  (format t
                          "Request to disconnect the client from the server received~%")
                  (as:write-socket-data sock (disconnect-message)
                                        :write-cb (lambda (socket)
                                                    (as:close-socket socket))))))))
     :connect-cb (lambda (sock)
                   (progn
                     (format t
                             "Connection to the server established.~%")
                     (as:write-socket-data sock (welcome-message)))))

    (as:signal-handler as:+sigint+
                       (lambda (sig)
                         (declare (ignore sig))
                         (as:exit-event-loop)))

    (format t "Test Server Initialized...~%")))


(cl-async:start-event-loop #'test-server)
