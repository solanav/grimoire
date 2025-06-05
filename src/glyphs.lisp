(in-package :grimoire)

(defvar *glyphs* (make-hash-table :test #'equal))

(defparameter *known-glyphs* 
  '(:sight
    :mark
    :command
    :sightless-command))

(defun glyph/exists? (glyph &key assert)
  (let ((res (when (member glyph *known-glyphs*) t)))
    (when assert (assert res)) res))

(define-condition missing-glyph (error)
  ((glyph :initarg :glyph
          :initform nil
          :reader glyph))
  (:report (lambda (condition stream)
             (format stream
                     "Please define-glyph that provides ~a"
                     (glyph condition))))
  (:documentation "System does not have a certain glyph"))

(defmacro define-glyph (glyph name args &body body)
  "create a function called `name` that provides `glyph` and takes `args`"
  (let ((function-name (create-function-name "~a/~a" glyph name)))
    `(progn (glyph/exists? ,glyph :assert t)
            (defun ,function-name ,args ,@body)
            (register ,glyph #',function-name))))

(defun glyph/list ()
  "return a list of glyphs available in the system"
  (a:hash-table-keys *glyphs*))

(defun glyph/info ()
  "show information about the glyphs available in the system"
  (loop for glyph being the hash-keys in *glyphs* using (hash-value function)
        do (out "[+] Glyph \"~a\"~%" glyph)
        do (out "    Provided by \"~a\"~%~%" function)))

(defun glyph/available? (glyph &key do-not-fail)
  "check if the glyph is available, fail otherwise"
  (let ((fun (gethash glyph *glyphs*)))
    (if fun fun 
        (if do-not-fail nil
            (error 'missing-glyph :glyph glyph)))))

(defmacro glyph/use (glyph &rest args)
  "use the glyph with the args"
  `(progn (glyph/exists? ,glyph :assert t)
          (funcall (can? ,glyph) ,@args)))

(defmacro glyph/add (glyph function)
  "register a function that gives a glyph"
  `(setf (gethash ,glyph *glyphs*) ,function))

;; alias for faster typing

(defmacro can? (glyph)
  `(glyph/available? ,glyph))

(defmacro use (glyph &rest args)
  `(glyph/use ,glyph ,@args))

(defmacro register (glyph function)
  `(glyph/add ,glyph ,function))
