(require :cffi)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :cl-glfw)
  (require :cl-glfw-opengl-version_1_1)
  (require :cl-glfw-glu))

(defpackage :v
  (:use :cl))

(in-package :v)

(cffi:load-foreign-library "./libav.so")
(cffi:defcfun "vid_libinit" :void)
(cffi:defcfun "vid_alloc" :uint64)
(cffi:defcfun "vid_free" :void (handle :uint64))
(cffi:defcfun "vid_init"
    :int
  (handle :uint64)
  (filename :string)
  (w :int)
  (h :int))

(cffi:defcfun "vid_decode_frame" 
    :int
  (handle :uint64))

(cffi:defcfun "vid_get_width" :int (handle :uint64))
(cffi:defcfun "vid_get_height" :int (handle :uint64))
(cffi:defcfun "vid_get_out_width" :int (handle :uint64))
(cffi:defcfun "vid_get_out_height" :int (handle :uint64))
(cffi:defcfun "vid_get_data" (:pointer :uint8) (handle :uint64) (i :int))
(cffi:defcfun "vid_get_linesize" :int (handle :uint64) (i :int))
(cffi:defcfun "vid_get_thread_count" :int (handle :uint64))
(cffi:defcfun "vid_get_thread_type" :int (handle :uint64))
(cffi:defcfun "vid_get_active_thread_type" :int (handle :uint64))
(cffi:defcfun "vid_set_active_thread_type" :void (handle :uint64) (i :int))

(cffi:defcfun "vid_close" :void (handle :uint64))
 


(defvar *h* nil)
(defvar *h2* nil)
(defvar *vids* nil)

(defparameter *vids*
  (mapcar #'first
	  (mapcar #'(lambda (x)
	    (list x
			     (with-open-file (s x)
			       (file-length s))))
		   (directory 
		    (merge-pathnames #p"*/*.*" "/dev/shm/")))
	  #+nil(sort 
	   
	   #'>
	   :key #'second)))

#+nil
(vid-libinit)

(let ((vids *vids*))
  (defun get-vids () vids)
 (defun grab-vid ()
   (if (= (length vids) 0)
       nil
       (let ((r (elt vids (random (length vids)))))
	 (setf vids
	       (set-difference vids (list r)))
	 r))))


(let ((t1 0d0)
      (t0 0d0)
      (frames 0))
 (defun count-fps ()
   (setf t1 (glfw:get-time))
   (when (or (< 1 (- t1 t0))
             (= frames 0))
     (glfw:set-window-title (format nil "bla ~,1f FPS"
                                    (/ frames (- t1 t0))))
     (setf frames 0
           t0 t1))
   (incf frames)))

(let ((rot 0)
      (start t))
  
  (defun draw ()
    (count-fps)
    (when start
      (setf start nil)
      (vid-libinit)
      (when *h*
	(loop for i below (length *h*) do
	   (vid-close (elt *h* i))))
      (progn
	(defparameter *h*
	  (loop for e in (loop for i below (min (* 10 5)
						
						(length *vids*)) collect
			      (grab-vid)) collect
	       (let ((h (vid-alloc)))
		 (format t "~a~%" (list e))
		 (let ((r (vid-init h (format nil "~a" e) 128 128)))
		   (loop while (< r 0)
		      do
			(setf r (vid-init h (format nil "~a" (grab-vid))
					  128 128))))
		 (vid-decode-frame h)
		 ;;(vid-set-active-thread-type h 1)
		 (format t "~a~%" (list (vid-get-width h) (vid-get-height h)
					(vid-get-thread-count h)
					(vid-get-thread-type h)
					(vid-get-active-thread-type h)))
		 h)))
	(format t "finished~%")))
    (destructuring-bind (w h) (glfw:get-window-size)
      (setf h (max h 1))
      (gl:viewport 0 0 w h)
      (gl:clear-color .0 .2 .2 1)
      (gl:clear (logior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
      (gl:matrix-mode gl:+projection+)
      (gl:load-identity)
;      (glu:perspective 65 (/ w h) 1 100)
      (gl:ortho 0 (* 10 128) (* 5 128) 0 .01 10)
      (gl:matrix-mode gl:+modelview+)
      (gl:load-identity)
      #+nil      (glu:look-at 0 1 10 ;; camera
			      0 0 0   ;; target
			      0 0 1))
    (gl:translate-f 0 0 -1)
    (gl:rotate-f 0 0 0 1)
    (if (< rot 360)
	(incf rot .3)
	(setf rot 0))
    (gl:with-push-matrix
      ;(gl:rotate-f rot 0 0 1)
     
      (let* ((objs (make-array (length *h*) :element-type '(unsigned-byte 32))))
	;(sleep (/ .3 60))
	(gl:gen-textures (length objs) objs)
	(dotimes (i  (length objs)) 
	  (gl:bind-texture gl:+texture-2d+ (aref objs i))
	  ;;(gl:pixel-store-i gl:+unpack-alignment+ 1)
	  (gl:tex-parameter-i gl:+texture-2d+ 
			      gl:+texture-min-filter+ gl:+linear+)
	  (gl:tex-parameter-i gl:+texture-2d+ 
			      gl:+texture-mag-filter+ gl:+linear+))
	(gl:enable gl:+texture-2d+)
	(gl:matrix-mode gl:+modelview+)
	(loop for i from 0 and h in *h* do
	     (when h
	       (let ((ww 128)
		     (hh 128))
		 (when (= 0 (vid-decode-frame h))
		   (format t "closing video ~a~%" h)
		   (vid-close h)
		   (let ((hnew (vid-alloc))
			 (fn (format nil "~a" (grab-vid))))
		     (format t "openingb ~a~%" fn)
		     (let ((r (vid-init hnew fn 128 128)))
		       (loop while (< r 0)
			  do
			    (setf r (vid-init hnew
					      (format nil "~a" (grab-vid))
					      128 128))))
		     ;;(vid-set-active-thread-type hnew 1)
		     (vid-decode-frame hnew)
		     (setf (elt *h* i) hnew)
		     (setf h hnew)))
		 (progn
		   (gl:bind-texture gl:+texture-2d+ (aref objs i))
		   
		   (gl:tex-image-2d gl:+texture-2d+ 0 
				    gl:+rgba+
				    128
				    128
				    0
				    #x80e1 ;; bgra 
				    ;;gl:+rgba+ 
				    gl:+unsigned-byte+
				    (vid-get-data h 0)))
		 (gl:with-push-matrix 
		   (let ((ii (mod i 6))
			 (jj (floor i 6)))
		     (gl:translate-f (* jj ww) (* ii hh) 0))
		   (gl:with-begin gl:+quads+
		     (labels ((c (a b)
				(gl:tex-coord-2f (/ a ww)  (/ b hh))
				(gl:vertex-2f a b)))
		       (c 0 0)
		       (c 0 hh)
		       (c ww hh)
		       (c ww 0)))))))
	
	(gl:disable gl:+texture-2d+)
	(gl:delete-textures (length objs) objs)))))



#+nil
(glfw:do-window (:title "bla" :width (* 10 128) :height (* 5 128))
    ()
  (when (eql (glfw:get-key glfw:+key-esc+) glfw:+press+)
    (return-from glfw::do-open-window))
  (draw))

