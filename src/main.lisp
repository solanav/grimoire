(in-package #:grimoire)

(defparameter *project-root* #P"~/documents/")

(defparameter *project-name* "linkvortex")
(defparameter *project-dir* (merge-pathnames 
                             (str:concat *project-name* "/")
                             *project-root*))

(defparameter *default-users* (load-data "default-users.txt"))
(defparameter *interesting-files* (load-data "interesting-files.txt"))

(defparameter *host-ip* "10.10.14.71")

(defparameter *capacities* (make-hash-table :test #'equal))

(setf *random-state* (make-random-state t))

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

(defmacro grm/use (type &rest args)
  `(funcall (gethash ,type *capacities*) ,@args))

(defun grm/register (type function)
  "register a function as a capacity"
  (setf (gethash type *capacities*) function))

(defun grm/read (path)
  "read a file"
  (grm/use :read path))

(defun grm/exec (command)
  "execute a command"
  (grm/use :exec command))