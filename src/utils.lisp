(in-package :grimoire)

(defun load-data (system-relative-path)
  "load a data/file.txt into a list"
  (remove-if #'alexandria:emptyp 
             (str:lines 
              (alexandria:read-file-into-string
               (asdf:system-relative-pathname
                :grimoire (merge-pathnames "data/" system-relative-path))))))

(defmacro grm/defun (name capacities parameters &body body)
  "create a function and check capacities at the start"
  `(defun ,name ,parameters
     ,@(loop for c in capacities
             collect `(grm/can ,c))
     
     ,@body))

(defun grm/can (type)
  "check if we have a capacity"
  (assert (not (null (gethash type *capacities*)))
          nil
          (format nil "Missing capacity for ~a" type)))

(defun grm/register (type function)
  "register a function as a capacity"
  (setf (gethash type *capacities*) function))

(defun grm/read-into-string (path)
  "read a file"
  (funcall *read-function* path))

(defun grm/download (path name)
  "read a file"
  (ignore-errors 
    (a:write-byte-vector-into-file 
     (funcall *read-function* path)
     (merge-pathnames 
      (asdf:system-relative-pathname
       :grimoire (merge-pathnames "output/" name))))
    t))
