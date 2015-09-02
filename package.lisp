;;;; package.lisp

(defpackage #:cl-dot
  (:use #:cl #:esrap)
  (:export
   #:id
   
   #:get-attribute
   #:set-attribute
   #:get-ancillary-attribute
   #:set-ancillary-attribute

   #:subgraph
   #:graph
   #:digraph
   #:node
   #:edge

   #:get-node-by-id))

