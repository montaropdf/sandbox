;; Test based on the Echo server example of the cl-async package.
;;
;; cl-sync repository: https://github.com/orthecreedence/cl-async

(ql:quickload "cl-async")
(ql:quickload "babel")
;; (ql:quickload "defenum")

;; (defenum:defenum *server-status* :tags-specs '('RUNNING 'SHUTTING-DOWN 'SHUTDOWN 'STARTED))

;; A set of macro to format some message to be sent to the clients
(defmacro shutdown-message ()
  (format nil "Request for shutdown received.~%You will be disconnected.~%Goodbye!~%"))

(defmacro disconnect-message ()
  (format nil "It was a pleasure to serve you, sir.~%See you soon.~%Goodbye!~%"))

(defmacro welcome-message ()
  (format nil "Welcome sir,~%How may I help you?~%"))
;; ------------------------------------------------------------------

(defparameter *socket-list* '()
  "List of sockets opened.")

(defparameter *server-status* :RUNNING
  "Overall status of the server.")

(defparameter *idlers* '()
  "List of known idlers.")
;; ------------------------------------------------------------------

(defun close-all-connections ()
  "Send a goodbye message and close all connection."
  (progn
    (dolist (sock *socket-list*)
      (as:write-socket-data
       sock (shutdown-message)
       :write-cb (lambda (socket)
                   (as:close-socket socket))))
    (format t "All connections closed.~%")))

(defun shutdown-server ()
  "Set the overall server status to SHUTTING-DOWN"
  (progn
    (setf *server-status* :SHUTTING-DOWN)
    (format t "*server-status* set to ~a~%" *server-status*)))

(defun clean-unused-sessions ()
  "Clean the socket list from any closed socket."
  (setf *socket-list*
        (remove-if #'(lambda (socket)
                       (as:socket-closed-p socket))
                   *socket-list*)))

(defun exit-if-shutdown ()
"Exit from the event loop if server status is shutting-down and the socket list is empty."
(when (and (null *socket-list*)
           (eql *server-status* :SHUTTING-DOWN))
  (progn
    (format t "Shutting Down...~%")
    (format t "Removing idler objects...~%")
    (dolist (idl *idlers*)
      (as:free-idler idl))
    (format t "Removing recurring event object...~%")
    (as:remove-interval *interval-closure*)
    (format t "Exiting from the Event Loop.~%")
    (as:exit-event-loop))))


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
         (format t "Socket: ~a~%Socket Data:~a~%" sock (as:socket-data sock))
         (format t "Length: ~a~%" (length data-as-string))
         ;; (format t "Last char: ~a~%"
         ;;         (last data-as-string)
         ;;         )
         (cond
           ((and (>  (length data-as-string) 13)
                 (string= data-as-string "(session open" :end1 12)
                 (char= (car (last data-as-string)) #\)))
            (progn
              (setf (as:socket-data sock)
                    (subseq data-as-string
                            12
                            (decf (length data-as-string))))
              (format t "Socket: ~a~%Socket Data:~a~%" sock (as:socket-data sock))
              ))
           ((string= data-as-string "(shutdown)")
            (progn
              ;; If (shutdown) is received, the server perform the
              ;; following steps:
              ;; 1. Send back a message to the client to inform him the server shuts down
              ;; 2. Close the socket
              ;; 3. Exit the event loop of cl-async (this will stop the program)
              (format t
                      "Request to shutdown the server received.~%")
              (close-all-connections)
              (shutdown-server)))
           ((string= data-as-string "(disconnect)")
            (progn
              ;; If (disconnect) is received, the server perform the
              ;; following steps:
              ;; 1. Send back a message to the client to acknowledge the client request a graceful disconnect
              ;; 2. Close the socket
              (format t
                      "Request to disconnect the client from the server received.~%")
              (as:write-socket-data sock (disconnect-message)
                                    :write-cb (lambda (socket)
                                                (as:close-socket socket))))))))
     :connect-cb (lambda (sock)
                   (progn
                     ;; Whenever a client open a connection, it will be greeted by a message.
                     (format t
                             "Connection to the server established.~%")
                     (format t "Socket: ~a~%Socket Data:~a~%" sock (as:socket-data sock))
                     (as:write-socket-data sock (welcome-message))
                     (push sock *socket-list*))))

    ;; Definition of a handler to intercept the SIGINT signal
    ;; This will exit the event loop and close the program
    (as:signal-handler as:+sigint+
                       (lambda (sig)
                         (declare (ignore sig))
                         (format t "SIGINT signal received.~%")
                         ;;(as:exit-event-loop)
                         (close-all-connections)
                         (shutdown-server)))

    ;; Setting some idlers
    (push (as:idle #'clean-unused-sessions) *idlers*)
    (push (as:idle #'exit-if-shutdown) *idlers*)

    ;; Displaying some statistics every 5 seconds.
    (setf *interval-closure*
          (as:interval (lambda ()
                         (format t
                                 "Number of elements in *socket-list*: ~a.~%"
                                 (length *socket-list*)))
                       :time 5))
    
    (format t "Test Server Initialized...~%")))

;; Start the event loop and initialize the server by calling the
;; function test-server
(cl-async:start-event-loop #'test-server)
(format t "Server closed.~%")
(format t "Application stopped.~%")
