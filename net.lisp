
(require :sb-bsd-sockets)
(defpackage :g (:use :cl :sb-bsd-sockets))
(in-package :g)


(defparameter soc (make-instance 'inet-socket 
				 :type :stream
				 :protocol :tcp))
(defparameter con (socket-connect soc #(192 168 1 100) #x5555))
