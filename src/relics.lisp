(in-package :grimoire)

(defvar *relics* (make-hash-table :test #'equal))

(defun relic/list ()
  (a:hash-table-alist *relics*))

(defun relic/add (key value)
  (out "Saved \"~a: ~a\" to the relic chest!~%" key value)
  (setf (gethash key *relics*) value))

(defun relic/get (key)
  (gethash key *relics*))
