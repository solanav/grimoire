(in-package :grimoire)

(defun read-bytes (stream length)
  (let ((res (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence res stream :end length)
    res))

(defun read-null-bytes (stream)
  "read bytes until they stop being null"
  (loop for b = (flexi-streams:peek-byte stream)
        while (= b 0)
        do (read-byte stream)))

(defun read-bytes-null-terminated (stream &key all)
  "read bytes until we find a null, if all read until no more nulls"
  (let ((res (make-array 0 :element-type '(unsigned-byte 8)
                         :adjustable t
                         :fill-pointer t)))

    ;; read bytes until the first null one
    (loop for byte = (read-byte stream)
          if (not (= byte 0))
          do (vector-push-extend byte res)
          until (= byte 0))

    ;; if instructed, read all nulls after the last one
    (when all (read-null-bytes stream))

    ;; return the vector
    res))

(defun bytes->number (bytes)
  (reduce
   (lambda (acc byte)
     (logior (ash acc 8) byte))
   bytes))

(defun bytes->hex (bytes)
  (ironclad:byte-array-to-hex-string bytes))

(defun bytes->string (bytes)
  (trivial-utf-8:utf-8-bytes-to-string bytes))

(defmacro parse-structure (stream structure)
  (if (listp structure)
      (let ((fun (car structure)) (body (cadr structure)))
        (cond ((eq fun :string)
               `(bytes->string (parse-structure ,stream ,body)))
              ((eq fun :bytes)
               `(read-bytes ,stream ,body))
              ((eq fun :until-last-nul)
               `(read-bytes-null-terminated ,stream :all t))
              ((eq fun :until-first-nul)
               `(read-bytes-null-terminated ,stream))
              ((eq fun :integer)
               `(bytes->number (parse-structure ,stream ,body)))
              ((eq fun :hex)
               `(bytes->hex (parse-structure ,stream ,body)))
              (t structure)))
      structure))

(defmacro let-bytes ((stream) grammar &body body)
  `(let ,(loop for (name structure) in grammar
               collect `(,name (parse-structure ,stream ,structure)))
     ,@body))
