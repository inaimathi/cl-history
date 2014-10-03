(in-package #:cl-history)

(defclass event ()
  ((timestamp :accessor timestamp :initform (local-time:now) :initarg :timestamp)
   (event-type :accessor event-type :initform :in :initarg :event-type)
   (data :accessor data :initform nil :initarg :data)))

(defmethod opposite ((ev event))
  (make-instance 
   'event
   :timestamp (timestamp ev) 
   :data (data ev)
   :event-type
   (case (event-type ev)
     (:in :out)
     (:out :in))))

(defgeneric tick (whole part)
  (:documentation "Should take a whole and a part. Should return the whole with the part added."))

(defgeneric wind (whole part)
  (:documentation "Should take a whole and a part. Should return the whole with the part removed."))

(defmethod apply-event (whole (ev event))
  (let ((part (data ev)))
    (case (event-type ev)
      (:in (tick whole part))
      (:out (wind whole part)))))

(defmethod rewind-event (whole (ev event))
  (apply-event whole (opposite ev)))

(defmethod load-from (empty (storage stream))
  (let ((res empty))
    (handler-case
	(loop 
	   do (setf res (tick res (cl-store:restore storage)))
	   do (read-byte storage))
      (end-of-file () res))))

(defmethod load-from (empty (fname pathname))
  (with-open-file (s fname :element-type '(unsigned-byte 8))
    (load-from empty s)))

(defmethod new-event! (whole (ev event) (storage stream) &rest more-streams)
  (loop for s in (cons storage more-streams)
     do (cl-store:store ev s)
     do (write-byte (char-code #\newline) s))
  (apply-event whole ev))
