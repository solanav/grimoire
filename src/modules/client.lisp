(in-package :grimoire)

(defparameter *client-host* "0.0.0.0")

(defparameter *buffer-size* 4096)
(defvar *max-payload-length* 4294967296)

(defvar *element-type* '(unsigned-byte 8))

(defun address->string (host)
  (format nil "~{~a~^.~}" (coerce host 'list)))

(defun fake-remote-shell-prompt (package host port)
  (out "~%~a@[~a:~a]> " package (address->string host) port)
  (force-output)
  (read-line))

(defun int->bytes (number n-bytes)
  "turn a number into a buffer of n-bytes in big endian"
  (let ((buff (make-array n-bytes :element-type *element-type*)))
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

(defun send-data (socket buffer)
  "send data and flush the stream"
  ;; (out "[+] Sending data: ~a~%" buffer)
  (let ((stream (usocket:socket-stream socket))
        (len (length buffer)))
    ;; payload cannot be bigger than 2**32 bytes (4GB)
    (assert (< len *max-payload-length*))

    ;; write the length of the payload
    (write-sequence (int->bytes len 4) stream)

    ;; write the payload
    (write-sequence buffer stream)

    (force-output stream)))

(defun read-payload-length (stream)
  ;; (out "[+] Checking payload length~%")
  (let ((buff (make-array 4)))
    (read-sequence buff stream)
    (bytes->int buff)))

(defun recv-data (socket)
  ;; (out "[+] Waiting for feedback from client...~%")
  (multiple-value-bind (sockets remaining-time)
      (usocket:wait-for-input socket :timeout 5)
    (declare (ignore sockets))
    (when (not remaining-time) 
      (error "Timeout waiting for feedback, closing connection")))
  

  ;; (out "[+] Starting to read...~%")

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
          ;; do (out "[+] Reading ~a to ~a. Total: ~a, left from payload: ~a~%"
          ;;         0 to-read total-read left-to-read)
          until (or (> i 10) (<= left-to-read 0))
          do (let ((read (read-sequence buffer stream :start 0 :end to-read)))
               (setf total-read (+ total-read read))
               (setf result (concatenate 'vector result (subseq buffer 0 read)))))

    (trivial-utf-8:utf-8-bytes-to-string result)))

(defun listen-on-available-port (host &key (start 10000) (end 10100))
  (loop for port from start upto end
        for socket = (ignore-errors (usocket:socket-listen host port))
        if socket do (return (values socket port))
        finally (error "[!] No port found")))

(defun remote-exec (connection command)
  (let ((buff (trivial-utf-8:string-to-utf-8-bytes
               command)))
    (send-data connection buff)
    (let* ((raw (recv-data connection))
           (res (jzon:parse raw)))
      ;; (out "[+] Received: ~a~%" raw)
      (values (gethash "result" res)
              (gethash "stdout" res)))))

(defun client/listen ()
  (multiple-value-bind (socket port) (listen-on-available-port *client-host*)
    (out "[+] Listening on ~a:~a~%" *client-host* port)

    (let* ((connection (usocket:socket-accept
                        socket :element-type *element-type*))
           (peer-addr (usocket:get-peer-name connection))
           (peer-port (usocket:get-peer-port connection)))

      (out "[+] Closing original socket...~%")
      (usocket:socket-close socket)

      (out "[+] Connection received from ~a:~a~%"
           peer-addr peer-port)

      ;; executi loop
      (loop for package = (remote-exec connection "(package-name *package*)")
            for command = (fake-remote-shell-prompt package peer-addr peer-port)
            for response = (multiple-value-list (remote-exec connection command))
            for result = (car response)
            for output = (cadr response)
            do (out "~{~a~%~}" (str:split #\Newline output))
            do (out "~S" result)
            until (or (and (not (car response)) 
                           (a:emptyp (cadr response)))
                      (string= command "(quit)")))

      ;; it may fail if the connection was closed by the peer
      (ignore-errors
        (usocket:socket-shutdown connection :io))

      (usocket:socket-close connection))))