(defsystem "grimoire"
  :version "0.0.1"
  :author "Antonio Solana"
  :license "AGPL"
  :depends-on ("alexandria"
               "serapeum"
               "str"
               "zip"
               "trivial-utf-8"
               "ironclad"
               "dexador"
               "com.inuoe.jzon"
               "hunchentoot"
               "easy-routes")
  :components ((:module "src"
                :serial t
                :components ((:file "package")

                             ;; Utils
                             (:file "utils")
                             (:file "bytes")
                             (:file "git")
                             (:file "server")
                             (:file "client")
                             (:file "main")

                             ;; Capabilities
                             (:module "capabilities"
                              :components ((:file "read")
                                           (:file "exec")))

                             ;; Machine specific functions
                             (:module "machines"
                              :components ((:file "titanic")
                                           (:file "linkvortex")
                                           (:file "code"))))))
  :description "Pentesting interactive framework")
