(in-package :grimoire)

;; utils

(defun url-append-file (uri file)
  (let* ((path (quri:uri-path uri)))
    (quri:make-uri
     :defaults uri
     :path (format nil "~a/~a"
                   (str:trim-right path :char-bag "/")
                   (str:trim-left file :char-bag "/")))))

(defun url-to-absolute (uri relative-link)
  (if (cl-ppcre:scan "(http|ftp|email|mail|ssh)s?:\/\/" relative-link)
      (quri:uri relative-link)
      (quri:make-uri
       :defaults uri
       :path (if (str:starts-with? "/" relative-link)
                 relative-link
                 (quri:uri-path (url-append-file uri relative-link))))))

;; functions

(defun crawler/links (uri)
  "extract the links of the URI's HTML"
  (let ((base-uri (quri:uri uri)))
    (loop :for found-url
          :across (lquery:$ (lquery:initialize (dex:get uri)) "a" (attr :href))
          :if (not (str:starts-with? "?" found-url))
          :collect (url-to-absolute base-uri found-url))))

(defun crawler/search (url term &key (max-recursion 3) (already-seen nil))
  "extracts the links in the URL and searches for the term in each one"
  (let ((links (list url)))
    (loop for next-link = (pop links))))