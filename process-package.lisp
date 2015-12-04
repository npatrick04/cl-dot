;;;; package.lisp

(defpackage #:cl-dot/process
  (:use #:cl #:cl-dot)
  (:export
   ;; The program to be called, default being "dot"
   ;;   "dot" - filter for drawing directed graphs
   ;;   "neato" - filter for drawing undirected graphs
   ;;   "twopi" - filter for radial layouts of graphs
   ;;   "circo" - filter for circular layout of graphs
   ;;   "fdp" - filter for drawing undirected graphs
   ;;   "sfdp" - filter for drawing large undirected graphs
   ;;   "patchwork" - filter for tree maps
   #:*dot-program*

   ;; Graph output types
   #:dot
   #:xdot

   ;; A user-specified type for generating files
   #:render-output

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

