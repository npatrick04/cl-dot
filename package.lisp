;;;; package.lisp

(defpackage #:cl-dot
  (:use #:cl #:accum #:iterate)
  (:import-from #:alexandria
                #:define-constant
                #:appendf)
  (:export
   #:*dot-print-type*
   #:read-dot
   #:read-dot-from-string
   #:read-dot-from-file

   ;; The reader macro, actually kind of cool
   #:set-dot-reader-macro

   #:subgraph
   #:graph
   #:digraph
   #:node
   #:edge

   #:id
   #:graph.env
   #:node.env
   #:edge.env
   #:contents
   #:graph-strict
   #:specific.env
   #:edges

   #:make-edges-in-subgraph
   #:get-nodes

   #:lookup-node-attribute
   #:node-attributes

   #:connector-style))

