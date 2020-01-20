(asdf:load-system :automata)

(in-package :automata.commands)

(automata.machinery:init-command-message-stream)

(duty 'test-exec1 "Test of removing a file"
  (exec "rm /home/roland/tmp/todel/.bash_profile~"))

(format t "~&Command Code: ~a~%" (automata.machinery:get-command-code))

(duty 'test-exec2 "Test of listing a directory"
  (exec "ls -alrt /home/roland/tmp/todel/"))

(format t "~&Command Code: ~a~%" (automata.machinery:get-command-code))

(duty 'test-copy "Test of file copy"
  (copy "/home/roland/.bash_profile~" "/home/roland/tmp/todel/"))

(format t "~&Command Code: ~a~%" (automata.machinery:get-command-code))

(duty 'test-progn "Test of grouping commands"
  (progn
    (exec "ls -alrt /home/roland/tmp/todel/")
    (exec "rm /home/roland/tmp/todel/.bash_profile~")
    (exec "ls -alrt /home/roland/tmp/todel/")))

(format t "~&Command Code: ~a~%" (automata.machinery:get-command-code))
