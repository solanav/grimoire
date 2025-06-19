(in-package :grimoire)

(defvar *nocturnal/host* "http://nocturnal.htb/")
(defvar *nocturnal/cookies* (cookie:make-cookie-jar))

(s:defalias nocturnal/path (s:partial #'s:concat *nocturnal/host*))

(defun nocturnal/register (username password)
  (dex:post (nocturnal/path "register.php")
            :content `(("username" . ,username)
                       ("password" . ,password))))

(defun nocturnal/login (username password)
  (dex:post (nocturnal/path "login.php")
            :cookie-jar *nocturnal/cookies*
            :content `(("username" . ,username)
                       ("password" . ,password))))

(defun nocturnal/upload ()
  (dex:post (nocturnal/path "dashboard.php")
            :cookie-jar *nocturnal/cookies*
            :content '(("fileToUpload" . #P"/tmp/fakepdf.pdf"))
            :verbose t))

(defun nocturnal/file (username file-name)
  (s:~> (quri:make-uri
         :defaults *nocturnal/host*
         :path "/view.php"
         :query `(("username" . ,username)
                  ("file" . ,file-name)))
        (dex:get :cookie-jar *nocturnal/cookies*)))