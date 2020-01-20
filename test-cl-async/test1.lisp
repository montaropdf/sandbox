(ql:quickload "cl-async")
(ql:quickload "babel")

;; A set of macro to format some message to be sent to the clients
(defmacro shutdown-message ()
  (format nil "Request of shutdown received.~%You will be disconnected.~%Goodbye!~%"))

(defmacro disconnect-message ()
  (format nil "It was a pleasure to serve you, sir.~%See you soon.~%Goodbye!~%"))

(defmacro welcome-message ()
  (format nil "Welcome sir,~%How may I help you?~%"))
;; ------------------------------------------------------------------

(defun test-server ()
  "This function will initialize the TCP Server and a handler on SIGINT to shutdown the server.

The Server have the following features:
- It can accept any TCP connection from locahost on port 6666.
- It understand the string (shutdown) and (disconnect).
- Any string different from the aboved will be sent back to the calling client.
- If (shutdown) is received, the server will shutdown and the program will stop.
- If (disconnect) is received, the server will close the socket with the client."
  (progn
    (format t "Initializing Test Server...~%")
    (cl-async:tcp-server
     "127.0.0.1" 6666
     (lambda (sock data)
       (let ((data-as-string (uiop:stripln
                              (babel:octets-to-string data))))

         ;; data-as-string contains the data received, converted to a
         ;; string and stripped from the end of line character
         (format t "Data received as string: ~a~%"
                 data-as-string)
         (format t "Raw data received: ~a~%" data)
         ;; Sending back the data to the client
         (as:write-socket-data sock data)
         
         (cond ((string= data-as-string "(shutdown)")
                (progn
                  ;; If (shutdown) is received, the server perform the
                  ;; following steps:
                  ;; 1. Send back a message to the client to inform him the server shuts down
                  ;; 2. Close the socket
                  ;; 3. Exit the event loop of cl-async (this will stop the program)
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
                  ;; If (shutdown) is received, the server perform the
                  ;; following steps:
                  ;; 1. Send back a message to the client to acknowledge the client request a graceful disconnect
                  ;; 2. Close the socket
                  (format t
                          "Request to disconnect the client from the server received~%")
                  (as:write-socket-data sock (disconnect-message)
                                        :write-cb (lambda (socket)
                                                    (as:close-socket socket))))))))
     :connect-cb (lambda (sock)
                   (progn
                     ;; Whenever a client open a connection, it will be greeted by a message
                     (format t
                             "Connection to the server established.~%")
                     (as:write-socket-data sock (welcome-message)))))

    ;; Definition of a handler to intercept the SIGINT signal
    ;; This will exit the event loop and close the program
    (as:signal-handler as:+sigint+
                       (lambda (sig)
                         (declare (ignore sig))
                         (as:exit-event-loop)))

    (format t "Test Server Initialized...~%")))

;; Start the event loop and initialize the server by calling the
;; function test-server
(cl-async:start-event-loop #'test-server)
