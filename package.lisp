;;;; package.lisp

(defpackage #:cl-history
  (:use #:cl)
  (:export #:event #:tick #:wind #:apply-event
	   #:load-from #:new-event!))

