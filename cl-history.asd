;;;; cl-history.asd

(asdf:defsystem #:cl-history
  :serial t
  :description "Describe cl-history here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:anaphora
               #:local-time
               #:cl-fad
	       #:cl-store)
  :components ((:file "package")
               (:file "cl-history")))
