(defsystem "grimoire"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ("alexandria"
               "serapeum"
               "str"
               "dexador"
               "trivial-utf-8"
               "ironclad")
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:file "bytes")
                             (:file "git")
                             (:file "main"))))
  :description "")
