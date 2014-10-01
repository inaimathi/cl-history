;;;; cl-history.lisp

(in-package #:cl-history)

(defclass event ()
  ((timestamp :accessor timestamp :initform (local-time:now) :initarg :timestamp)
   (event-type :accessor event-type :initform :in :initarg :event-type)
   (data :accessor data :initform nil :initarg :data)))

(defgeneric tick (whole part)
  (:documentation "Should take a whole and a part. Should return the whole with the part added."))

(defgeneric wind (whole part)
  (:documentation "Should take a whole and a part. Should return the whole with the part removed."))

(defmethod apply-event (whole (ev event))
  (let ((part (data ev)))
    (case (event-type ev)
      (:in (tick whole part))
      (:out (wind whole part))
      (:batch (reduce 
	       (lambda (memo pair)
		 (destructuring-bind (old new) pair
		   (tick (wind memo old) new)))
	       part :initial-value whole)))))

(defmethod load-from (empty (fname pathname))
  (let ((res empty))
    (with-open-file (s fname :element-type '(unsigned-byte 8))
      (handler-case
	  (loop 
	     do (setf res (tick res (cl-store:restore s)))
	     do (read-byte s))
	(end-of-file () res)))))
