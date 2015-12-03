;;;; package.lisp

(defpackage #:cl-dot/process
  (:use #:cl #:cl-dot)
  (:export
   #:*dot-program*

   ;; Graph output types
   #:dot
   #:xdot

   ;; File output types
   #:dot-file
   #:xdot-file
   #:ps-file
   #:svg-file
   #:svgz-file
   #:fig-file
   #:png-file
   #:gif-file
   #:imap-file
   #:cmapx-file))

