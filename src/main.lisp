(in-package :grimoire)

;; to be filled by (setup)
(defvar *project-name* nil)
(defvar *host-ip* nil)
(defvar *project-dir* nil)

;; to be customized by dev
(defparameter *project-root* (uiop:native-namestring "~/documents/htb/"))

;; global stuff
(defvar *default-users* (load-data "default-users.txt"))
(defvar *interesting-files* (load-data "interesting-files.txt"))
(defvar *capabilities* (make-hash-table :test #'equal))
(defvar *derivations* (make-hash-table :test #'equal))
(defvar *loot* (make-hash-table :test #'equal))

(setf *random-state* (make-random-state t))

(defstruct derivation
  name from to function)

(define-condition missing-capability (error)
  ((capability :initarg :capability
               :initform nil
               :reader capability))
  (:report (lambda (condition stream)
             (format stream
                     "Please define-capability that provides ~a"
                     (capability condition))))
  (:documentation "System does not have a certain capability"))

(defun setup ()
  "setup everything to start hacking"
  (setf *project-name* (prompt "Project name"))
  (setf *host-ip* (prompt "Host IP"))

  ;; derived
  (setf *project-dir*
        (merge-pathnames
         (str:concat *project-name* "/")
         *project-root*))

  nil)

;; TODO
(defun try-deriving (needed-capability)
  (let ((capabilities (a:hash-table-keys *capabilities*)))
    (flet ((need-to (c) (eq needed-capability c))
           (able-to (c) (member c capabilities)))
      (cond ((and (need-to :read) (able-to :exec))
             (derive-read-from-exec))))))

;; main utilities

(defun is-use (expr)
  "check if an expression is a use of a capability and return which one"
  (when (and (eq (first expr) 'use)
             (member (second expr) '(:read :exec :write)))
    (second expr)))

(defmacro find-requirements (&body body)
  "find the required capabilities in the body of a function"
  (labels ((aux (expr)
             (when (and expr (listp expr))
               (cons (is-use expr)
                     (append (aux (car expr))
                             (aux (cdr expr)))))))
    `(list ,@(remove-if
              #'null (loop for expr in body
                           append (aux expr))))))

(defmacro define-recipe (name capabilities args &body body)
  "create a recipe called `name` that needs `capabilities` and takes `args`"
  `(defun ,name ,args
     ,@(loop for c in capabilities
             collect `(can? ,c))
     ,@body))

;; TODO: register calls to other capabilities to find cicles in future derivations
(defmacro define-capability (capability name args &body body)
  "create a function called `name` that provides `capabilities` and takes `args`"
  (let ((function-name (create-function-name "~a/~a" capability name)))
    `(if (find-requirements ,body)
         (error "You cannot use capabilities inside other capabilities")
         (progn (defun ,function-name ,args ,@body)
                (register ,capability #',function-name)))))

(defmacro define-derivation (name (from to) &body body)
  (let ((string-name (string-upcase name)))
    `(setf (gethash ,string-name *derivations*)
           (make-derivation
            :name ,string-name
            :from ,from
            :to ,to
            :function (lambda (f) ,@body)))))

(find-requirements
 (define-capability :exec planning (command)
   (let ((output "/tmp/grafana-cmd-output"))
     (planning/query (fmt *planning/exec-query* command output))
     (use :read output))))

(define-derivation cat (:exec :read)
  (use :exec (fmt "cat ~a" f)))

(defun loot (&optional key value)
  "save some loot to later use it or retrieve it if no value is provided"
  (cond ((and key value)
         (out "Saved \"~a: ~a\" to the loot chest!~%" key value)
         (setf (gethash key *loot*) value))
        (value (error "Cannot have value but no key!"))
        (t (a:hash-table-alist *loot*))))

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