(asdf:defsystem "golem"
  :version "0.0.1"
  :author "Antonio Solana"
  :license "AGPL"
  :depends-on ("usocket"
               "trivial-utf-8"
               "com.inuoe.jzon")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "golem"
  :entry-point "golem::entry")

;; Deploy may not find libcrypto on your system.
;; But anyways, we won't ship it to rely instead
;; on its presence on the target OS.
(require :cl+ssl)  ; sometimes necessary.
#+linux (deploy:define-library cl+ssl::libssl :dont-deploy T)
#+linux (deploy:define-library cl+ssl::libcrypto :dont-deploy T)

;; ASDF wants to update itself and fails.
(deploy:define-hook (:deploy asdf) (directory)
  #+asdf (asdf:clear-source-registry)
  #+asdf (defun asdf:upgrade-asdf () NIL))
