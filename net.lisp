
(require :sb-bsd-sockets)
(defpackage :g (:use :cl :sb-bsd-sockets))
(in-package :g)

(defvar *stm* nil)
#+nil
(progn
  (defparameter soc (make-instance 'inet-socket 
				   :type :stream
				   :protocol :tcp))
  (defparameter con (socket-connect 
		     soc #(192 168 1 100) #x5555))

  (defparameter *stm* (socket-make-stream soc 
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
				       &key)
  (with-slots (packet-type cmd1 cmd2 flags payload-length 
			   data-payload
			   checksum) p
    (setf payload-length (length data-payload))
    (setf checksum (mod (+ packet-type cmd1 cmd2 flags payload-length
			   (reduce #'+ data-payload)) #x100))))


(defun ascii-code-p (num)
  (<= (char-code #\Space) num (1+ (char-code #\}))))

(defun are-numbers-ascii-p (payload)
  "This is a simple heuristic, to print strings."
  (let* ((chunk (remove-trailing-zeros payload))
	 (ascii (remove-if-not #'ascii-code-p chunk)))
    ;; when ascii and chunk have the same size, we look at a string
    ;; otherwise, chunk will be bigger
    (unless (= 0 (length chunk)) 
      (< .9 (/ (length ascii)
	       (length chunk))))))

(defmethod print-object ((obj dlp-packet) str)
  (with-slots (packet-type cmd1 cmd2 flags payload-length 
			   data-payload checksum) obj
    (format str "<pkt~{ ~a~}>" (list (parse-packet-type packet-type)
				     cmd1 cmd2
				     (parse-flags flags)
				     ;payload-length
				     data-payload
				     #+nil(if (are-numbers-ascii-p data-payload)
					 (convert-payload-to-string data-payload)
					 data-payload) 
				     checksum
				     ;(parse-packet-all-types obj)
				     ))))


(defun remove-trailing-zeros (ls)
  (let ((trailing-zeros (loop for i from (1- (length ls)) downto 0
			   when (= 0 (elt ls i)) count 1)))
    (subseq ls 
		0 
		(- (length ls) 
		   trailing-zeros))))


(defun convert-payload-to-string (payload)
  (map 'string #'code-char
	(remove-trailing-zeros payload)))

(defun make-host-read-command (cmd1 cmd2 &key data)
  (make-instance 'dlp-packet :packet-type 4 :cmd1 cmd1 :cmd2 cmd2
		 :flags 0 :data-payload data))

(defun make-host-write-command (cmd1 cmd2 &key data)
  (make-instance 'dlp-packet :packet-type 2 :cmd1 cmd1 :cmd2 cmd2
		 :flags 0 :data-payload data))

(defun read-version (system)
  (make-host-read-command 1 0 :data (list (ecase system
					    (arm 0)
					    (fpga #x10)
					    (msp #x20)))))
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


(defmacro with-tcp (&body body)
  `(let ((p ,@body)
	 (stream *stm*))
     (write-sequence (convert-to-sequence p) stream)
     (force-output stream)
     (read-available-bytes stream)))

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

(defun parse-error-payload (err)
  (ecase err
    (1 'unknown)
    (2 'invalid-command)
    (3 'invalid-parameter)
    (4 'out-of-memory)
    (5 'hardware-fail)
    (6 'hardware-busy)
    (7 'not-initialized)
    (8 'object-not-found)
    (9 'checksum-error)
    (#xa 'packet-format-error)
    (#xb 'continuation-error)))

(defmethod parse-type-error-response ((p dlp-packet) request)
  (with-slots (packet-type cmd1 cmd2 payload-length flags
			   data-payload) p
    (unless (= packet-type 1)
      (error "no error response packet"))
    (when request
     (unless (and (= cmd1 (cmd1 request))
		  (= cmd2 (cmd2 request)))
       (error "response cmd1 and cmd2 do not match request packet.")))
    (unless (= 1 payload-length)
      (error "response doesn't contain the expected byte."))
    (parse-error-payload (elt data-payload 0))))

(defmethod parse-packet-all-types ((p dlp-packet) &optional request)
  "Dispatch to different functions depending on the packet type. We do
extra checks if you supply the request packet, when P is an
answer---of type 0, 1, 3 or 5."
  (ecase (packet-type p)
    ;(0 (parse-type-busy-response p request))
    (1 (parse-type-error-response p request))
    ;(2 (parse-type-write p))
    ;(3 (parse-type-write-response p request))
    ;(4 (parse-type-read p))
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


(defun read-video-mode-setting ()
  (let ((resp (with-tcp (make-host-read-command 2 1))))
    (make-dlp-packet-from-sequence resp)))
#+nil
(defparameter *vid-set* (read-video-mode-setting))
;; 60Hz 8bit 1rgb

(defun write-video-mode-setting (freq bits color)
  (declare (type (integer 1 8) bits)
	   (type (integer 1 4) color))
  (let ((resp (with-tcp (make-host-write-command 2 1 :data 
						     (list freq bits color)))))
    (make-dlp-packet-from-sequence resp)))
#+nil
(defparameter *rep* (write-video-mode-setting 60 8 3))

(defun set-current-display-mode (mode)
  (declare (type (integer 0 4) mode))
  (let ((resp (with-tcp (make-host-write-command 1 1 :data 
						 (list mode)))))
    (make-dlp-packet-from-sequence resp)))

#+nil
(set-current-display-mode 2)

(defun read-solution ()
  (let ((resp (with-tcp (make-host-read-command 6 0))))
    (make-dlp-packet-from-sequence resp)))

#+nil
(read-solution) ;; by default no solutions are stored

(defun string-to-padded-list (name)
  (declare (type (simple-array character 1) name))
  (let ((n (length name)))
    (unless (<= 1 n 32)
      (error "name must have at most 32 characters")))
  (let* ((zero-padded (map-into (make-array 32 :element-type '(unsigned-byte 8)
					    :initial-element 0)
				#'(lambda (x) (char-code x))
				name)))
    (map 'list #'identity zero-padded)))

(defun save-solution (name)
  (let* ((resp (with-tcp (make-host-write-command 6 0 
						  :data (string-to-padded-list name)))))
    (make-dlp-packet-from-sequence resp)))

#+nil
(save-solution "hdmi")

(defun set-default-solution (name)
  (let* ((resp (with-tcp 
		 (make-host-write-command 
		  6 1
		  :data (append '(2) (string-to-padded-list name))))))
    (make-dlp-packet-from-sequence resp)))

#+nil
(set-default-solution "hdmi")

#+nil
(defparameter *led* (read-led-current-setting))
(defun read-led-current-setting ()
  (let ((resp (with-tcp (make-host-read-command 1 4))))
    (make-dlp-packet-from-sequence resp)))
; 18 1 18 1 18 1 ; data is always little endian,  12 1 would be default (274, 633mA)

(defun read-dlpc3000-register (reg)
  (let ((resp (with-tcp (make-host-read-command #xff 0 :data (list reg)))))
    (make-dlp-packet-from-sequence resp)))

(defun get-led-power ()
 (loop for r in '(#x12 #x13 #x14) collect ;; rgb led brightness settings, 750 is default
      (destructuring-bind (a b c d)
	  (data-payload (read-dlpc3000-register r))
	(+ a (* b 256)))))

#+nil
(get-led-power)
#+nil
(progn
 (defparameter *dither* (read-dlpc3000-register #x7e))
 (defparameter *agc* (read-dlpc3000-register #x50))
 (defparameter *leda* (read-dlpc3000-register #x4b))
 (defparameter *led3* (read-dlpc3000-register #x16)))

(defun ensure-list-length (ls len)
  (subseq ls 0 (min 4 (length ls))))
#+nil
(list
 (ensure-list-length '(1 2 3 4 5) 4)
 (ensure-list-length '(1 2 3) 4))

(defun write-dlpc3000-register (reg data)
  (let ((resp (with-tcp
		(make-host-write-command #xff 0
					 :data 
					 (append (list reg)
						 (ensure-list-length data 4))))))
    (make-dlp-packet-from-sequence resp)))

#+nil
(defparameter *bla*
 (write-dlpc3000-register #x16 '(#b101)))
#+nil
(defparameter *bla2*
 (write-dlpc3000-register #x4b '(#x14 #x15 #x16 #x24)))

#+nil
(write-dlpc3000-register #x4b '(0 0 0 0))

#+nil
(progn
  (read-dlpc3000-register #x82)		; sequencer

 (read-dlpc3000-register #x1e)		; vsync
 (read-dlpc3000-register #x83)		; seq 0 1 0 0
 (read-dlpc3000-register #x3a)		; busy
 (read-dlpc3000-register #x7e)		; dither
 )
(defun disable-video-processing ()
  (list
   (write-dlpc3000-register #x50 '(6 0 0 0)) ;; agc
   (write-dlpc3000-register #x7e '(2 0 0 0)) ;; temporal dither
   (write-dlpc3000-register #x5e '(0 0 0 0)) ;; color coordinate adjustment
   ))

#+nil
(disable-video-processing)

; (read-dlpc3000-register #x0c) ;; resolution 35 1:1 mirror mapping landscape
(defun set-led-power (r &optional (g r) (b r))
  (declare (type (integer 750 #x400) r g b))
  (loop for (reg val) in `((#x12 ,r) (#x13 ,g) (#x14 ,b)) collect
       (let ((msb (ldb (byte 3 8) val)) ;; send 11 bit value
	     (lsb (ldb (byte 8 0) val)))
	(write-dlpc3000-register reg (list lsb msb 0 0)))))

#+nil
(set-led-power 1024)

(defun program-new-sequence (seq)
  (unless (= 0 (first (data-payload (read-dlpc3000-register #x3a))))
    (error "busy.")) ;; only continue if not busy
  (write-dlpc3000-register #x39 (list seq 0 0 0)) ;; sequence number
  (write-dlpc3000-register #x3a '(1 0 0 0))  ;; set busy
  (write-dlpc3000-register #x38 '(#xc1 0 0 0))  ;; command to load new sequence
  (sleep .1)
  (unless (= 0 (first (data-payload (read-dlpc3000-register #x3a))))
    (error "shouldn't be busy anymore.")))

#+nil
(program-new-sequence 12) ;; 24 times 1bit mono

#+nil
(program-new-sequence 3)

#+nil
(list
 (read-dlpc3000-register #x16)
 (read-dlpc3000-register #x4c)
 (read-dlpc3000-register #x4b))
#+nil
(list
 (write-dlpc3000-register #x16 '(#b111 0 0 0))
 (write-dlpc3000-register #x4c '(#x13 #x12 #x00 #x00))  ;(#x13 #x12 #x11 #x25)
 (write-dlpc3000-register #x4b '(#x00 #x00 #x00 #x14))) ;(#x24 #x16 #x15 #x14)
#+nil
(progn
  (write-dlpc3000-register #x50 '(0 0 0 6)) ; agc off
 (write-dlpc3000-register #x7e '(0 0 0 2)) ; temporal dither off
 (write-dlpc3000-register #x5e '(0 0 0 0)) ; color coordinate off
 (write-dlpc3000-register #x1e '(0 0 0 1)) ; lock to vsync
 (write-dlpc3000-register #x16 '(0 0 0 3)) ; red
 (write-dlpc3000-register #x4b '(#x14 #x15 #x16 #x24)) ;
 (write-dlpc3000-register #x4b '(#x25 #x11 #x12 #x13)) ;


 (read-dlpc3000-register #xb)
 )
