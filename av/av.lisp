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
 


(defvar *h* nil) ;; contains a list of currently played videos
(defvar *vids* nil) ;; contains a list of all available videos

#+nil
(defparameter *vids*
  (mapcar #'first
	  (mapcar #'(lambda (x)
	    (list x
			     (with-open-file (s x)
			       (file-length s))))
		   (directory 
		    (merge-pathnames #p"*.*" "/dev/shm/r/")))))

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
     (glfw:set-window-title (format nil "~,1f FPS"
                                    (/ frames (- t1 t0))))
     (setf frames 0
           t0 t1))
   (incf frames)))

(declaim (inline threshold-pixel-into-rgba))
(defun threshold-pixel-into-rgba (v j col)
  (declare (type (unsigned-byte 32) col)
	   (type (integer 0 23) j)
	   (type (unsigned-byte 8) v))
  (the (unsigned-byte 32)
   (setf (ldb (byte 1 j) col)
	 (if (< 127 v) 1 0))))


(let* ((start t)
       (vw 960)
       (vh 540)
       (dec 1)
       (dw (floor vw dec))
       (dh (floor vh dec)))
  
  (defun open-new-video ()
    (let ((h (vid-alloc)) ;; h will be freed when vid-close is
	  ;; called in decode-frame
	  )
      (loop with r = -1
	 do 
	   (setf r (vid-init h
			     (format nil "~a" (grab-vid))
			     dw dh))
	 while (< r 0))
      ;;(vid-set-active-thread-type hnew 1)
      (vid-decode-frame h)
      (format t "~a~%" (list (vid-get-width h) (vid-get-height h)
			     (vid-get-thread-count h)
			     (vid-get-thread-type h)
			     (vid-get-active-thread-type h)))
      h))
  
  (defun load-all-videos ()
    (setf start nil)
    (vid-libinit)
    (when *h*
      (loop for i below (length *h*) do
	   (vid-close (elt *h* i))))
    (defparameter *h*
      (loop for e in (loop for i below (min 1
					    (length *vids*)) collect
			  (grab-vid)) collect
	   (open-new-video))))
  (defun setup-gl-coordinate-system ()
    (destructuring-bind (w h) (glfw:get-window-size)
      (setf h (max h 1))
      (gl:viewport 0 0 w h)
      (gl:matrix-mode gl:+projection+)
      (gl:load-identity)
      (gl:ortho 0 dw dh 0 .01 10)
      (gl:matrix-mode gl:+modelview+)
      (gl:load-identity)))
  (defun create-gl-texture-objects (n)
    (let ((objs (make-array n :element-type '(unsigned-byte 32))))
      (gl:gen-textures (length objs) objs)
      (dotimes (i  (length objs)) 
	(gl:bind-texture gl:+texture-2d+ (aref objs i))
	;;(gl:pixel-store-i gl:+unpack-alignment+ 1)
	(gl:tex-parameter-i gl:+texture-2d+ 
			    gl:+texture-min-filter+ gl:+nearest+)
	(gl:tex-parameter-i gl:+texture-2d+ 
			    gl:+texture-mag-filter+ gl:+nearest+))
      objs))
  
  (defun decode-frame (h)
    (when (= 0 (vid-decode-frame h))
      (format t "closing video ~a~%" h)
      (vid-close h)
      (setf h (open-new-video)))
    h)
  (defun copy-frame-to-texture (h obj)
    (gl:bind-texture gl:+texture-2d+ obj)
    (gl:tex-image-2d gl:+texture-2d+ 0 
		     gl:+rgba+
		     dw
		     dh
		     0
					; #x80e1 ;; bgra
		     gl:+rgba+ 
		     gl:+unsigned-byte+
		     (vid-get-data h 0)))
  (defun draw-quad (w h)
    (gl:with-begin gl:+quads+ 
      (labels ((c (a b)
		 (gl:tex-coord-2f (/ a w)  (/ b h))
		 (gl:vertex-2f a b)))
	(c 0 0)
	(c 0 h)
	(c w h)
	(c w 0))))
  (defun draw ()
    (gl:clear-color .9 .2 .2 1)
    (gl:clear (logior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
      
    (count-fps)
    (when start
      (load-all-videos))
    (setup-gl-coordinate-system)
    (gl:translate-f 0 0 -1)
					;    (gl:scale-f (sqrt 2) 1 1)
    (gl:rotate-f 0 0 0 1)

    (let* ((objs (create-gl-texture-objects (length *h*))))
      (gl:enable gl:+texture-2d+)
      (loop for i from 0 and h in *h* do
	   (when h
	     (setf h (decode-frame h))
	     (copy-frame-to-texture h (aref objs i))
	     (draw-quad dw dh)))
      
      (gl:disable gl:+texture-2d+)
      (gl:delete-textures (length objs) objs))))



#+nil
(glfw:do-window (:title "bla" :width (floor 960 5) :height (floor 540 5))
    ()
  (when (eql (glfw:get-key glfw:+key-esc+) glfw:+press+)
    (return-from glfw::do-open-window))
  (draw))

