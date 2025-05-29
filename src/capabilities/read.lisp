(in-package :grimoire)

(define-recipe system-info (:read) ()
  "read system information"
  (parse-key=value (use :read "/etc/os-release")))

(define-recipe all-users (:read) ()
  "list all the users from /etc/passwd"
  (loop for line in (str:lines (use :read "/etc/passwd"))
        if (not (alexandria:emptyp line))
        collect (car (str:split ":" line))))

(define-recipe users (:read) ()
  "list the users that are not default"
  (remove-if 
   #'(lambda (u) (member u *default-users* :test #'string=))
   (all-users)))

(define-recipe flag (:read) (&key username root)
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