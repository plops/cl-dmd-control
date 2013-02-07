
(require :sb-bsd-sockets)
(defpackage :g (:use :cl :sb-bsd-sockets))
(in-package :g)


(defparameter soc (make-instance 'inet-socket 
				 :type :stream
				 :protocol :tcp))
(defparameter con (socket-connect 
		   soc #(192 168 1 100) #x5555))

(defparameter stm (socket-make-stream soc 
				      :input t
				      :output t
				      :element-type '(unsigned-byte 8)))

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
    (when (listp data-payload)
      (setf data-payload (make-array (length data-payload) 
				     :element-type '(unsigned-byte 8)
				     :initial-contents data-payload)))
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

(make-instance 'dlp-packet :)

(defmacro with-tcp (stream &body body)
  `(let ((p ,@body))
     (write-sequence p ,stream)
     (force-output ,stream)
     (let ((recv (read-available-bytes ,stream)))
       (push (list p
		   (let ((recv-a (map-into (make-array (length recv)
						       :element-type '(unsigned-byte 8))
					   #'identity
					   recv)))
		     (unless (= #xda (aref recv-a 10))
		       (error "packet wasn't successful: ~a"
			      (list (format-packet p)
				    (format-packet recv-a))))
		     recv-a))
	     *resp*))
     (loop for (send recv) in (list (first *resp*)) collect
	  (list
	   (format-packet send)
	   (format-packet recv)))))7
