(in-package :grimoire)

(defvar *transmutations* (make-hash-table :test #'equal))

(defstruct transmutation
  name needs provides function)

(defmacro define-transmutation (name args (&rest needs) (&rest provides) &body body)
  (let ((string-name (string-upcase name)))
    `(progn
       (defun ,name ,args
         ,@body)
       (setf (gethash ,string-name *transmutations*)
             (make-transmutation
              :name ,string-name
              :needs ',needs
              :provides ',provides
              :function #',name)))))

(defun transmutation/runnable? (transmutation)
  (let ((glyphs (glyph/list)))
    (loop for need in (transmutation-needs transmutation)
          always (member need glyphs))))

(defun transmutation/needed? (transmutation)
  (let ((glyphs (glyph/list)))
    (loop for need in (transmutation-provides transmutation)
          never (member need glyphs))))

(defun transmutation/get (name)
  (gethash name *transmutations*))

(defun transmutation/list ()
  (a:hash-table-keys *transmutations*))

(defun transmutation/info ()
  (loop for name being the hash-keys in *transmutations*
        using (hash-value transmutation)
        do (out "[+] Transmutation \"~a\"~%" 
                (transmutation-name transmutation))
        do (out "    Runnable? ~a (needs ~{:~a~^, ~})~%" 
                (yes? (transmutation/runnable? transmutation))
                (transmutation-needs transmutation))
        do (out "    Needed?   ~a (provides ~{:~a~^, ~})~%~%"
                (yes? (transmutation/needed? transmutation))
                (transmutation-provides transmutation))))

(defun transmutation/run (name)
  (let ((transmutation (gethash (string-upcase name) *transmutations*)))
    (loop for provides in (transmutation-provides transmutation)
          do (glyph/add provides (transmutation-function transmutation))
          do (out "[+] Added new glyph: :~a~%" provides))))

(define-transmutation cat (file)
    (:command) (:sight)
  "use sighted exec to cat a file"
  (glyph/use :command (fmt "cat ~a" file)))

(define-transmutation let-there-be-light (command)
    (:sight :sightless-command) (:command)
  "dump output of the command to a tmp file and then read the output"
  (let ((temp-file "/tmp/grimoire.tmp"))
    (glyph/use :sightless-command (fmt "~a > ~a" command temp-file))
    (glyph/use :sight temp-file)))
