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

(defun reset ()
  (setf *project-name* nil)
  (setf *host-ip* nil)
  (setf *glyphs* (make-hash-table :test #'equal))
  (setf *rituals* (make-hash-table :test #'equal))
  (setf *relics* (make-hash-table :test #'equal))
  
  (out "[+] System reset completed succesfully~%"))
