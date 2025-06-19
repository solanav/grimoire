(in-package :grimoire)

(defvar *phishing/kits-location* 
  (asdf:system-relative-pathname :grimoire "data/kits/"))

(defvar *phishing/store* "POYE223NPXS8GBVO18TGLP7VSBSMHTXK")

(defun phishing/disk-kit-path (kit)
  "get the absolute path of a kit"
  (merge-pathnames kit *phishing/kits-location*))

(defun phishing/disk-kit-files (path)
  "list the files inside a kit in disk"
  (let ((kits nil))
    (ignore-errors 
      (zip:with-zipfile (kit path)
        (zip:do-zipfile-entries (name entry kit)
          (out "[+] ~a~%" name)
          ;; (let ((contents (zip:zipfile-entry-contents entry)))
          ;;   (when (plusp (length contents))
          ;;     (push (s:dict "name" name
          ;;                   "hash" (bytes-sha256 contents))
          ;;           kits)))
          )))
    kits))

(defun phishing/disk-kit-paths ()
  "list the paths of the kits in disk"
  (mapcar #'phishing/disk-kit-path 
          (uiop:directory-files *phishing/kits-location*)))

(defun phishing/kits (&key force)
  "list the kits"
  (loop with paths = (phishing/disk-kit-paths)
        with total = (length paths)
        
        for path in paths
        for i = 0 then (1+ i)
        for path-name = (namestring path)
        
        do (out "[+] [~a/~a] Processing kit \"~a\"~%" i total path)
        if (or force (not (store/exists *phishing/store* path-name)))
        do (store/set
            *phishing/store*
            path-name 
            (phishing/disk-kit-files path)
            :json t)))

(defun phishing/kit-files (kit)
  (store/get *phishing/store* kit :json t))

(defun phishing/search-in-kit (kit hash)
  (find hash (phishing/kit-files kit) 
        :key (lambda (ht) (ignore-errors (gethash "hash" ht)))
        :test #'string=))

(defun phishing/search (hash)
  (loop for key in (store/keys *phishing/store*)
        for res = (phishing/search-in-kit key hash)
        if res do (progn 
                    (out "[+] Found hash in kit \"~a\", file \"~a\"~%"
                         key (gethash "name" res))
                    (return res))))
