(in-package #:cl-history)

(defclass base-archive ()
  ((cur-id :accessor cur-id :initform 0)
   (zero :reader zero :initarg :zero)
   (current :accessor current :initarg :current)
   (file :reader file :initarg :file :initform nil)))

(defclass event ()
  ((id :reader id :initarg :id)
   (timestamp :accessor timestamp :initform (local-time:now) :initarg :timestamp)
   (payload :reader payload :initarg :payload)))

(defmethod current-id ((arc base-archive)) (cur-id arc))

(defmethod insert! ((arc base-archive) data)
  (let ((ev (make-instance 'event :id (incf (cur-id arc)) :payload data)))
    (when (file arc)
      (with-open-file (s (file arc) :direction :output :if-exists :append :if-does-not-exist :create :element-type '(unsigned-byte 8))
	(cl-store:store ev s)))
    (insert-event! arc ev)))

(defmethod insert-event! ((arc base-archive) (ev event)) 
  (setf (current arc) (apply-payload (current arc) (payload ev))
	(cur-id arc) (max (cur-id arc) (id ev)))
  nil)

(defmethod update! ((arc base-archive) (id integer) dat)
  (let ((reconciled (loop with d = dat while d
		       for ev in (events-since arc id)
		       do (setf d (reconcile d (payload ev)))
		       finally (return d))))
    (when reconciled 
      (insert! arc reconciled))))

(defmethod load-from! ((empty base-archive) (storage stream))
  (handler-case 
      (loop do (insert-event! empty (cl-store:restore storage)))
    (end-of-file () empty)))

(defmethod load-from! ((empty base-archive) (storage pathname))
  (with-open-file (s storage :element-type '(unsigned-byte 8))
    (load-from! empty s)))

(defmethod load-from! ((empty base-archive) (storage string))
  (load-from! empty (pathname storage)))

;;;;;;;;;; Standard Archive
(defclass archive (base-archive)
  ((events :accessor events :initform nil)))

(defun mk-archive (zero &key file)
  (make-instance 'archive :zero zero :current zero :file file))

(defmethod project ((arc archive) (id integer))
  (if (= id (cur-id arc))
      (current arc)
      (reduce #'apply-payload (events arc) :from-end t :key #'payload :initial-value (zero arc))))

(defmethod events-since ((arc archive) (id integer))
  (reverse
   (loop for ev in (events arc)
      while (> (id ev) id) 
      collect ev)))

(defmethod events-since ((arc archive) (ts local-time:timestamp))
  (reverse
   (loop for ev in (events arc) 
      while (local-time:timestamp> (timestamp ev) ts)
      collect ev)))

(defmethod insert-event! ((arc archive) (ev event))
  (push ev (events arc))
  (setf (current arc) (apply-payload (current arc) (payload ev))
	(cur-id arc) (max (cur-id arc) (id ev)))
  nil)

;;;;;;;;;; User-defined stuff
(defgeneric reconcile (a b)
  (:documentation "Define your own reconcilable method to use the update! archive method. 
It needs to take two instances, `a` and `b`, of whatever data class you're using and return one of
  - a copy of `a` ahat reflects any changes that should be applied if `b` happened after `a` was created, but before it was applied to an archive
  - `a` (if no change is necessary as a result of `b` happening first)
  - NIL (if `b` removes the need to apply `a` at all) ."))

(defgeneric apply-payload (whole part)
  (:documentation "Define your own apply-payload method to use any part of cl-history.
It needs to take a `whole` (the complete object you're building up) and a `part` (the chunklet contained inside an individual event), and non-destructively apply `part` to `whole` (that is, it may return `whole` unaltered, if there was no consequence of `part`, but if any changes result from the application, it may not destructively modify `whole`)."))
