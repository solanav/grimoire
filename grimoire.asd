(defsystem "grimoire"
  :version "0.0.1"
  :author "Antonio Solana"
  :license "AGPL"
  :depends-on ("alexandria"
               "serapeum"
               "str"
               "zip"
               "sqlite"
               "trivial-utf-8"
               "ironclad"
               "dexador"
               "com.inuoe.jzon"
               "hunchentoot"
               "easy-routes"
               "lparallel")
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             
                             ;; utils
                             (:file "utils")
                             
                             ;; core
                             (:file "relics")
                             (:file "glyphs")
                             (:file "spells")
                             (:file "transmutations")

                             ;; modules
                             (:module "modules"
                              :components 
                              ((:file "bytes")
                               (:file "sqlite3")
                               (:file "git")
                               (:file "server")
                               (:file "client")))
                             
                             ;; Main
                             (:file "main"))))
  :description "Pentesting interactive framework")
