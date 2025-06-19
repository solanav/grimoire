(in-package :grimoire)

(defvar *store* (sqlite:connect 
                 (asdf:system-relative-pathname
                  :grimoire "data/store.db")) )

;; table handlers

(defun store/create ()
  "create a new partition in the store"
  (let ((id (string-upcase (random-string 32))))
    (sqlite:execute-non-query 
     *store* (fmt "CREATE TABLE ~a (key TEXT PRIMARY KEY, value JSON NOT NULL)" 
                  id))
    id))

(defun store/delete (id)
  "delete the store"
  (sqlite:execute-non-query
   *store* (fmt "DROP TABLE ~a" id)))

(defun store/reset (id)
  "empty the data in a store"
  (sqlite:execute-non-query
   *store* (fmt "DELETE FROM ~a" id)))

(defun store/list ()
  "get all the stores"
  (s:~>> (sqlite:execute-to-list
          *store* "SELECT name FROM sqlite_master WHERE type='table'")
         (mapcar #'car)))

;; object handlers

(defun store/keys (id)
  "list all the keys in a store"
  (s:~>> (sqlite:execute-to-list
          *store* (fmt "SELECT key FROM ~a" id))
         (mapcar #'car)))

(defun store/set (id key value &key json)
  "set a key/value pair inside a store"
  (sqlite:execute-non-query
   *store* (fmt "INSERT OR REPLACE INTO ~a (key, value) VALUES (?, ?)" id)
   key (if json (jzon:stringify value) value))
  value)

(defun store/get (id key &key json)
  "get the value assigned to key in the store"
  (let ((value (sqlite:execute-single
                *store* (fmt "SELECT value FROM ~a WHERE key = ?" id)
                key)))
    (if json (jzon:parse value) value)))

(defun store/exists (id key)
  (not (null (store/get id key))))