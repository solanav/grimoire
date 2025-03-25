(in-package :grimoire)

(defparameter *acceptor* 
  (make-instance
   'easy-routes:easy-routes-acceptor
   :address "0.0.0.0"
   :port 5000))

(defparameter *tool-dir* 
  (merge-pathnames
   "tools/" (asdf:system-source-directory :grimoire)))

(defparameter *downloads-dir*
  (merge-pathnames
   "downloads/" (asdf:system-source-directory :grimoire)))

(easy-routes:defroute proxy ("/proxy/" :method :get) (url)
  "acts as a simple http proxy"
  (multiple-value-bind (body status headers uri stream)
      (dex:get url)
    (declare (ignore status uri stream))
    (setf (hunchentoot:content-type*) (gethash "content-type" headers))
    body))

(easy-routes:defroute tools ("/tools/:tool-name" :method :get) ()
  (setf (hunchentoot:content-type*) "application/octet-stream")
  (a:read-file-into-byte-vector (merge-pathnames tool-name *tool-dir*)))

(easy-routes:defroute uploads ("/upload/" :method :post) ()
  "saves uploaded files to *downloads-dir*"
  (let ((file (hunchentoot:post-parameter "file"))
        (buff (make-array 4096 :element-type '(unsigned-byte 8))))
    (format t "[+] Downloading file ~a to ~a~%" 
            (car file)
            (merge-pathnames (cadr file) *downloads-dir*))
    (with-open-file (input (car file)
                           :direction :input
                           :element-type '(unsigned-byte 8))
      (ensure-directories-exist *downloads-dir*)
      (with-open-file (output (merge-pathnames (cadr file) *downloads-dir*)
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))
        (loop for bytes = (read-sequence buff input)
              while (plusp bytes)
              do (write-sequence buff output :end bytes))
        "ok"))))

(defun start-server ()
  (hunchentoot:start *acceptor*))

(defun stop-server ()
  (hunchentoot:stop *acceptor*))

(defun restart-server ()
  (stop-server)
  (start-server))
