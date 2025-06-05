(in-package :grimoire)

;; to be filled
(defvar *project-name* nil)
(defvar *host-ip* nil)
(defvar *project-dir* nil)

(defparameter *project-root*
  (uiop:native-namestring "~/documents/htb/"))

;; global stuff
(defvar *default-users*
  (load-data "default-users.txt"))
(defvar *interesting-files*
  (load-data "interesting-files.txt"))

(setf *random-state* (make-random-state t))

(defun separator (&optional str (length 50))
  (flet ((tmp (n) (make-string n :initial-element #\=)))
    (let* ((new-total (- length (length str)))
           (title (string-upcase str))
           (left (floor (/ new-total 2)))
           (right (if (evenp new-total) left (1+ left))))
      (if str 
          (out "~a ~a ~a~%~%" (tmp (1- left)) title (tmp (1- right)))
          (out "~a~%~%" (tmp length))))))

(defun info ()
  "show information about the system's current state"
  (separator "~* spells ~*")
  (spell/info)
  
  (separator "~> transmutations ~>")
  (transmutation/info)

  (separator "<> glyphs <>")
  (glyph/info)
  
  (separator))

(defun start ()
  "setup everything to start hacking"
  (setf *project-name* (prompt "Project name"))
  (setf *host-ip* (prompt "Host IP"))

  ;; derived
  (setf *project-dir*
        (merge-pathnames
         (str:concat *project-name* "/")
         *project-root*))

  (out "[+] Setup complete~%"))

(defun forget ()
  "forget all accumulated knowledge"
  (setf *project-name* nil)
  (setf *host-ip* nil)
  
  (setf *glyphs* (make-hash-table :test #'equal))
  (setf *spells* (make-hash-table :test #'equal))
  (setf *relics* (make-hash-table :test #'equal))
  
  (out "[+] Grimoire forgot all... You can start anew.~%"))
