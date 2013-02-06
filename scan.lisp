(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload '(cl-glfw cl-opengl cl-glu)))

(declaim (optimize (debug 3) (speed 1) (safety 3)))

(defpackage :disp
  (:use :cl :gl))

(in-package :disp)

(let ((rot 0))
 (defun draw ()
   (sleep (/ 64))
   (destructuring-bind (w h) (glfw:get-window-size)
     (progn
       (viewport 0 0 w h)
       (matrix-mode :projection)
       (load-identity)
       (glu:ortho-2d 0 w 0 h)
       (matrix-mode :modelview)
       (clear :color-buffer-bit)
       (clear-color 0 0 0 0)
       (line-width 1)
       (color 0 1 0)
       #+nil
       (let ((period 2))
	 (flet ((v (x)
		  (vertex x 0) (vertex x h)))
	   (with-pushed-matrix
	     (translate (floor w 4) (floor h 4) 0)
	     (rotate -45 0 0 1)
	     (with-primitive :lines
	       (dotimes (i 100) 
		  (dotimes (j period)
		    (v (+ (* .5 (sqrt 2) j)
			  (* period (+ (* (sqrt 2) i)))))))))
	   (with-pushed-matrix
	     (translate (floor w 4) (floor h 4) 0)
	     (rotate 45 0 0 1)
	     (with-primitive :lines
	       (dotimes (i 100) 
		  (dotimes (j period)
		    (v (+ (* .5 (sqrt 2) j)
			  (* period (+ (* (sqrt 2) i)))))))))))
       (with-primitive :points
	 (vertex 200 200)
	 (vertex 203 200)
	 (vertex 206 200)
	 (vertex 200 205)
	 (vertex 202 205)
	 (vertex 200 210)
	 (vertex 201 210)
	 (vertex 200 215)
	 (vertex 200 216)
	 (vertex 11 10)
	 (vertex 11 14)
	 (vertex 12 14)
	 (vertex 11 19)
	 (vertex 12 19)
	 (vertex 13 19))

       (with-pushed-matrix
	(translate 10 10 0)
	(with-primitive :points
	  (dotimes (i 20)
	    (dotimes (j 50)
	      (vertex (* 4 i) (* 4 j))))))
       (incf rot 1)))))

#+nil
(glfw:do-window (:title "display grid on projector" 
			:width 608 :height 342)
    ((glfw:swap-interval 1))
  (when (eql (glfw:get-key glfw:+key-esc+) glfw:+press+)
    (return-from glfw::do-open-window))
  (draw))
