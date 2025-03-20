(in-package :grimoire)

(defparameter *acceptor* 
  (make-instance
   'easy-routes:easy-routes-acceptor
   :address "0.0.0.0"
   :port 5000))

(defparameter *tool-dir* 
  (merge-pathnames
   "tools/" (asdf:system-source-directory :grimoire)))

(easy-routes:defroute proxy ("/proxy" :method :get) (url)
  "acts as a simple http proxy"
  (multiple-value-bind (body status headers uri stream)
      (dex:get url)
    (declare (ignore status uri stream))
    (setf (hunchentoot:content-type*) (gethash "content-type" headers))
    body))

(easy-routes:defroute tools ("/tools/:tool-name") ()
  (setf (hunchentoot:content-type*) "application/octet-stream")
  (a:read-file-into-byte-vector (merge-pathnames tool-name *tool-dir*)))

(defun start-server ()
  (hunchentoot:start *acceptor*))

(defun stop-server ()
  (hunchentoot:stop *acceptor*))
