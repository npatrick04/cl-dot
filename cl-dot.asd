;;;; cl-dot.asd

(asdf:defsystem #:cl-dot
  :description "An interface to graphviz."
  :author "Nick Patrick <npatrick04@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (:esrap)
  :components ((:file "package")
               (:file "cl-dot")
	       (:file "read-dot")))

