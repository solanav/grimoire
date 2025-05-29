(in-package :grimoire)

;; to be filled
(defvar *project-name* nil)
(defvar *host-ip* nil)
(defvar *project-dir* nil)

(defvar *capabilities* (make-hash-table :test #'equal))
(defvar *derivations* (make-hash-table :test #'equal))
(defvar *loot* (make-hash-table :test #'equal))

;; to be customized by dev
(defparameter *project-root* (uiop:native-namestring "~/documents/htb/"))

;; global stuff
(defvar *default-users* (load-data "default-users.txt"))
(defvar *interesting-files* (load-data "interesting-files.txt"))

(setf *random-state* (make-random-state t))

(define-condition missing-capability (error)
  ((capability :initarg :capability
               :initform nil
               :reader capability))
  (:report (lambda (condition stream)
             (format stream
                     "Please define-capability that provides ~a"
                     (capability condition))))
  (:documentation "System does not have a certain capability"))

;; dev utils

(defun setup ()
  "setup everything to start hacking"
  (setf *project-name* (prompt "Project name"))
  (setf *host-ip* (prompt "Host IP"))

  ;; derived
  (setf *project-dir*
        (merge-pathnames
         (str:concat *project-name* "/")
         *project-root*))

  (out "[+] Setup complete~%"))

(defun reset ()
  (setf *project-name* nil)
  (setf *host-ip* nil)
  (setf *capabilities* (make-hash-table :test #'equal))
  (setf *derivations* (make-hash-table :test #'equal))
  (setf *loot* (make-hash-table :test #'equal))
  
  (out "[+] System reset completed succesfully~%"))

;; recipies

(defmacro define-recipe (name capabilities args &body body)
  "create a recipe called `name` that needs `capabilities` and takes `args`"
  `(defun ,name ,args
     ,@(loop for c in capabilities
             collect `(can? ,c))
     ,@body))

;; capabilities

(defmacro define-capability (capability name args &body body)
  "create a function called `name` that provides `capabilities` and takes `args`"
  (let ((function-name (create-function-name "~a/~a" capability name)))
    `(progn (defun ,function-name ,args ,@body)
            (register ,capability #',function-name))))

(defun capability/list ()
  (a:hash-table-keys *capabilities*))

;; derivations

(defstruct derivation
  name needs provides function)

(defmacro define-derivation (name args (&rest needs) (&rest provides) &body body)
  (let ((string-name (string-upcase name)))
    `(setf (gethash ,string-name *derivations*)
           (make-derivation
            :name ,string-name
            :needs ',needs
            :provides ',provides
            :function (lambda ,args ,@body)))))

(defun derivation/runnable? (derivation)
  (let ((capabilities (a:hash-table-keys *capabilities*)))
    (loop for need in (derivation-needs derivation)
          always (member need capabilities))))

(defun derivation/needed? (derivation)
  (let ((capabilities (a:hash-table-keys *capabilities*)))
    (loop for need in (derivation-provides derivation)
          never (member need capabilities))))

(defun derivation/get (name)
  (gethash name *derivations*))

(defun derivation/list ()
  (loop for name being the hash-keys in *derivations*
        using (hash-value derivation)
        do (out "[+] Derivation \"~a\"~%" 
                (derivation-name derivation))
        do (out "    Runnable? ~a (needs ~{:~a~^, ~})~%" 
                (yes? (derivation/runnable? derivation))
                (derivation-needs derivation))
        do (out "    Needed?   ~a (provides ~{:~a~^, ~})~%~%"
                (yes? (derivation/needed? derivation))
                (derivation-provides derivation))))

(defun derivation/run (name)
  (let ((derivation (gethash (string-upcase name) *derivations*)))
    (loop for provides in (derivation-provides derivation)
          do (register provides (derivation-function derivation))
          do (out "[+] Added new capability: :~a~%" provides))))

;; loot

(defun loot/list ()
  (a:hash-table-alist *loot*))

(defun loot/add (key value)
  (out "Saved \"~a: ~a\" to the loot chest!~%" key value)
  (setf (gethash key *loot*) value))

(defun loot/get (key)
  (gethash key *loot*))

;; utils

(defun can? (capability)
  "check if the capability is available, fail otherwise"
  (let ((fun (gethash capability *capabilities*)))
    (if fun fun (error 'missing-capability :capability capability))))

(defmacro use (capability &rest args)
  "use the capability with the args"
  `(funcall (can? ,capability) ,@args))

(defun register (capability function)
  "register a function that gives a capability"
  (setf (gethash capability *capabilities*) function))