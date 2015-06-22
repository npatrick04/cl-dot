;;;; cl-dot.asd

(asdf:defsystem #:cl-dot
  :description "An interface to graphviz."
  :author "Nick Patrick <npatrick04@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "cl-dot")))

