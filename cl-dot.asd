;;;; cl-dot.asd

(asdf:defsystem #:cl-dot
  :description "An interface to graphviz."
  :author "Nick Patrick <npatrick04@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (:named-readtables :split-sequence)
  :components ((:file "package")
	       (:file "environment")
               (:file "id")
               (:file "cl-dot")
	       (:file "read")
               (:file "eval")
               ;; (:file "print")
               ))

