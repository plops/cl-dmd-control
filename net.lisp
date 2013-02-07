
(require :sb-bsd-sockets)
(defpackage :g (:use :cl :sb-bsd-sockets))
(in-package :g)

#+nil
(progn
  (defparameter soc (make-instance 'inet-socket 
				   :type :stream
				   :protocol :tcp))
  (defparameter con (socket-connect 
		     soc #(192 168 1 100) #x5555))

  (defparameter stm (socket-make-stream soc 
					:input t
					:output t
					:element-type '(unsigned-byte 8))))

(defun read-byte-no-hang (stream)
  (when (listen stream)
    (read-byte stream)))

(defun read-available-bytes (stream)
  (let* ((b (read-byte stream))
	 res)
    (loop while b do
	 (push b res)
	 (setf b (read-byte-no-hang stream)))
    (reverse res)))

;; documentation of the protocol is in dlpu007c.pdf
;; which is available from texas instruments

(defclass dlp-packet ()
  ((packet-type :accessor packet-type 
		:initarg :packet-type
		:type (unsigned-byte 8))
   (cmd1 :accessor cmd1 
   	 :initarg :cmd1
   	 :type (unsigned-byte 8))
   (cmd2 :accessor cmd2 
   	 :initarg :cmd2
   	 :type (unsigned-byte 8))
   (flags :accessor flags 
   	  :initarg :flags
	  :initform 0
   	  :type (and  (unsigned-byte 8)
		      (member 0 ; complete
			      1 ; beginning 
			      2 ; intermediate
			      3 ; end
			      )))
   (payload-length :accessor :payload-length
   		   :initarg payload-length
		   :initform 0
   		   :type (unsigned-byte 16))
   (data-payload :accessor data-payload 
   		 :initarg :data-payload
   		 :type (or null
			   (simple-array (unsigned-byte 8) 1)))
   (checksum :accessor checksum 
   	     :initarg :checksum
	     :type (unsigned-byte 8))))

(defmethod initialize-instance :after ((p dlp-packet) 
				       &key (packet-type #x04)
					 data-payload)
  (with-slots (packet-type cmd1 cmd2 flags payload-length 
			   data-payload
			   checksum) p
    (setf payload-length (length data-payload))
    (setf checksum (mod (+ packet-type cmd1 cmd2 flags payload-length
			   (reduce #'+ data-payload)) #x100))))

(defmethod print-object ((obj dlp-packet) str)
  (with-slots (packet-type cmd1 cmd2 flags payload-length 
			   data-payload checksum) obj
    (format str "<pkt~{ ~a~}>" (list packet-type cmd1 cmd2 flags
				     payload-length 
				     data-payload checksum))))

(defun make-host-read-command (cmd1 cmd2 &key data)
  (make-instance 'dlp-packet :packet-type #x4 :cmd1 cmd1 :cmd2 cmd2
		 :flags 0 :data-payload data))

(defun read-versions ()
  (make-host-read-command 1 0 :data '(0)))
#+nil
(read-versions)

#+nil
(convert-to-sequence (read-versions))

(defmethod convert-to-sequence ((p dlp-packet))
  (with-slots (packet-type cmd1 cmd2 flags payload-length 
			   data-payload checksum) p
    ;; lsb of payload-length comes first
    (let* ((lsb (ldb (byte 8 0) payload-length))
	   (msb (ldb (byte 8 8) payload-length))
	   (l (append (list packet-type cmd1 cmd2 flags lsb msb)
		      data-payload
		      (list checksum))))
      (make-array (length l)
			   :element-type '(unsigned-byte 8)
			   :initial-contents l))))
#+nil
(write-sequence (convert-to-sequence (read-versions))
		stm)
#+nil
(force-output stm)


(defmacro with-tcp (stream &body body)
  `(let ((p ,@body))
     (write-sequence (convert-to-sequence p) ,stream)
     (force-output ,stream)
     (read-available-bytes ,stream)))

#+nil
(defparameter recv (with-tcp stm
		     (read-versions)))

(make-dlp-packet-from-sequence recv)

(defun make-dlp-packet-from-sequence (seq)
  (let* ((payload-length (+ (elt seq 4)
			    (* (elt seq 5) #x100)))
	 (p (make-instance 'dlp-packet 
			 :packet-type (elt seq 0)
			 :cmd1 (elt seq 1)
			 :cmd2 (elt seq 2)
			 :flags (elt seq 3)
			 :data-payload (subseq seq 6 (+ 6 payload-length)))))
    (unless (= (length (data-payload p))
	       payload-length)
      (error "length doesn't match"))
    (unless (= (elt seq (+ 6 payload-length))
	       (checksum p))
      (error "checksum test failed."))
    p))
