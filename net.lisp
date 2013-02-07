
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
#+nil
(progn
 (socket-close soc)
 (setf stm nil))

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
    (format str "<pkt~{ ~a~}>" (list (parse-packet-type packet-type)
				     cmd1 cmd2
				     (parse-flags flags)
				     ;payload-length 
				     (convert-payload-to-string data-payload) 
				     checksum))))

(defun convert-payload-to-string (payload)
 (let ((trailing-zeros (loop for i from (1- (length payload)) downto 0
			   when (= 0 (elt payload i)) count 1)))
   (map 'string #'code-char
	(subseq payload 
		0 
		(- (length payload) 
		   trailing-zeros)))))

(defun make-host-read-command (cmd1 cmd2 &key data)
  (make-instance 'dlp-packet :packet-type #x4 :cmd1 cmd1 :cmd2 cmd2
		 :flags 0 :data-payload data))

(defun read-version (system)
  (make-host-read-command 1 0 :data (list (ecase system
					    ('arm 0)
					    ('fpga #x10)
					    ('msp #x20)))))
#+nil
(read-version 'arm)

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
		     (read-version 'msp)))

#+nil
(make-dlp-packet-from-sequence recv)

(defun parse-flags (flags)
  (ecase flags
    (0 'singlepac)
    (1 'multi-beg)
    (2 'multi-mid)
    (3 'multi-end)))

(defmethod parse-type-read-response ((p dlp-packet) request)
  (with-slots (packet-type cmd1 cmd2 payload-length flags) p
    (unless (= packet-type 5)
      (error "no read response packet"))
    (unless (and (= cmd1 (cmd1 request))
		 (= cmd2 (cmd2 request)))
      (error "response cmd1 and cmd2 do not match request packet."))
    (when (= 0 payload-length)
      (error "response doesn't contain data."))
    ))

(defun parse-packet-type (type)
  (ecase type
    (0 'busy)
    (1 'erro)
    (2 'wreq)
    (3 'wans)
    (4 'rreq)
    (5 'rans)))

(defmethod parse-packet-type ((p dlp-packet) &optional request)
  "Dispatch to different functions depending on the packet type. We do
extra checks if you supply the request packet, when P is an
answer---of type 0, 1, 3 or 5."
  (ecase (packet-type p)
    (0 (parse-type-busy p request))
    (1 (parse-type-error p request))
    (2 (parse-type-write p))
    (3 (parse-type-write-response p request))
    (4 (parse-type-read p))
    (5 (parse-type-read-response p request))))



(defun make-dlp-packet-from-sequence (seq)
  (let* ((payload-length (+ (elt seq 4)
			    (* (elt seq 5) #x100)))
	 (p (make-instance 'dlp-packet 
			 :packet-type (elt seq 0)
			 :cmd1 (elt seq 1)
			 :cmd2 (elt seq 2)
			 :flags (elt seq 3)
			 :data-payload (subseq seq 6 (+ 6 payload-length)))))
    (unless (= (elt seq (+ 6 payload-length))
	       (checksum p))
      (error "checksum test failed."))
    p))
