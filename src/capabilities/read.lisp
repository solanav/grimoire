(in-package #:grimoire)

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
               (remove-if #'null)
               (remove-if #'a:emptyp)))))