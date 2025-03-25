(defsystem "grimoire"
  :version "0.0.1"
  :author "Antonio Solana"
  :license "AGPL"
  :depends-on (;; Generic utils
               "alexandria"
               "serapeum"
               "str"
               "zip"
               "trivial-utf-8"

               ;; Crypto
               "ironclad"
               
               ;; Networking
               "dexador"
               "com.inuoe.jzon"
               
               ;; For serving files after getting access
               "hunchentoot"
               "easy-routes")
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             
                             ;; Utils
                             (:file "utils")
                             (:file "bytes")
                             (:file "git")
                             (:file "web")

                             ;; Capabilities
                             (:module "capabilities"
                              :components ((:file "read")
                                           (:file "exec")))
                             
                             ;; Machine specific functions
                             (:module "machines"
                              :components ((:file "titanic")
                                           (:file "linkvortex")))

                             ;; Main module
                             (:file "main"))))
  :description "Pentesting interactive framework")
