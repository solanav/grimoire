(in-package :grimoire)

(defun load-data (system-relative-path)
  "load a data/file.txt into a list"
  (remove-if #'alexandria:emptyp 
             (str:lines 
              (alexandria:read-file-into-string
               (asdf:system-relative-pathname
                :grimoire (merge-pathnames "data/" system-relative-path))))))

(defun project-path (filename)
  "gives the project-relative file path"
  (merge-pathnames filename *project-dir*))

(defun random-string (length)
  (let ((charset "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
    (coerce 
     (loop for i upto (1- length)
           collect (elt charset (random (length charset))))
     'string)))
