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
       (clear-color 1 1 1 1)
       (clear :color-buffer-bit)
       
       (line-width 1)
       
       (color 0 0 0 1)

       #+nil (color (expt (sin (/ rot 4.2)) 2) 
	      (acos (expt (sin (/ rot 3.2)) 2))
	      (expt (sin (/ rot 8.3)) 2))
      
       (let ((period 8)
	     (width 15))
	 (flet ((v (x)
		  (vertex x 0) (vertex x 1000)))
	   (with-pushed-matrix
	     (scale 1 2 1)
	     (translate (floor w 4) (floor h 4) 0)
	     (rotate -45 0 0 1)
	     ;; perhaps, in order to show more complicated images with
	     ;; their pixels accurately mapped to the DMD, i should
	     ;; write them into a texture and then display the texture
	     ;; at -45 degree orientation
	     (with-primitive :lines
	       (dotimes (i 200) 
		  (dotimes (j width)
		    (v (+ (* .5 (sqrt 2) j)
			  ;(* .5 (sqrt 2) (mod rot (* 2 period)))
			  (* period (* (sqrt 2) i ))
			  ))))))
	   #+nil(with-pushed-matrix
	     (scale 1 2 1)
	     (translate (floor w 4) (floor h 4) 0)
	     (rotate 45 0 0 1)
	     (with-primitive :lines
	       (dotimes (i 200) 
		  (dotimes (j width)
		    (v (+ (* .5 (sqrt 2) j)
			   ;(* .5 (sqrt 2)  (mod rot period))
			  (* period (* (sqrt 2) i))))))))))
       (with-pushed-matrix
	 (scale 1 2 1)
	 (translate 10 10 0)
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
	   (vertex 1 0)
	   (vertex 1 4)
	   (vertex 2 4)
	   (vertex 1 8)
	   (vertex 2 8)
	   (vertex 3 8))

	(with-pushed-matrix
	  
	  (with-primitive :points
	    (dotimes (i 20)
	      (dotimes (j 50)
		(vertex (* 4 i) (* 4 j)))))))
       (incf rot 1)))))

#+nil
(glfw:do-window (:title "display grid on projector" 
			:width 608 :height 342)
    ((glfw:swap-interval 1))
  (when (eql (glfw:get-key glfw:+key-esc+) glfw:+press+)
    (return-from glfw::do-open-window))
  (draw))
