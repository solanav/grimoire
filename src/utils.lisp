(in-package :grimoire)

(defvar *alphanumeric* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(defmacro out (control-string &rest format-arguments)
  `(format t ,control-string ,@format-arguments))

(defmacro fmt (control-string &rest format-arguments)
  `(format nil ,control-string ,@format-arguments))

(defun gethash-or-elt (key obj)
  "access the element no matter if its a hashtable or a vector"
  (cond ((hash-table-p obj) (ignore-errors (gethash key obj)))
        ((and (typep obj 'sequence) (numberp key)) (ignore-errors (elt obj key)))
        (t nil)))

(defun @2 (obj &rest keys)
  "index deeply nested vectors/hash-tables"
  (let ((next-value (gethash-or-elt (car keys) obj)))
    (if (= (length keys) 1)
        next-value
        (apply #'@2 next-value (cdr keys)))))

(defun load-data (system-relative-path)
  "load a file located in the data/ folder into a list"
  (s:~>> (asdf:system-relative-pathname :grimoire "data/")
         (merge-pathnames system-relative-path)
         (a:read-file-into-string)
         (str:lines)
         (remove-if #'a:emptyp)))

(defun project-path (project-relative-path)
  "gives the project-relative file path"
  (merge-pathnames project-relative-path *project-dir*))

(defun random-string (length &optional charset)
  (let ((charset (or charset *alphanumeric*)))
    (coerce 
     (loop for i upto (1- length)
           collect (elt charset (random (length charset))))
     'string)))

(defmacro create-function-name (control-string &rest format-arguments)
  `(intern (string-upcase (fmt ,control-string ,@format-arguments))))

(defun prompt (message)
  (out "~a:~%>>> " message)
  (force-output)
  (read-line))

(defun parse-key=value (text)
  "parse line separated string with key=value into a list of cons"
  (loop for line in (str:lines text)
        for (head . tail) = (str:split "=" line)
        collect (cons head (str:join "=" tail))))
