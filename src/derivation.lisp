(in-package :grimoire)

(define-derivation cat (file)
    (:exec) (:read)
  "use sighted exec to cat a file"
  (use :exec (fmt "cat ~a" file)))

(define-derivation let-there-be-light (command) 
    (:read :blind-exec) (:exec)
  "dump output of the command to a tmp file and then read the output"
  (let ((temp-file "/tmp/grimoire.tmp"))
    (use :blind-exec (fmt "~a > ~a" command temp-file))
    (use :read temp-file)))
