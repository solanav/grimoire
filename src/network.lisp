(in-package :grimoire)

(defun brute-force (url method usernames passwords)
  (loop for username in usernames
        do (loop for password in passwords
                 do (dex:request url 
                                 :method method
                                 :content `(("username" . ,username)
                                            ("password" . ,password))))))
