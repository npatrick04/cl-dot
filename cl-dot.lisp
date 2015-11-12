;;;; cl-dot.lisp

;;; Copyright 2015 Nick Patrick

(in-package #:cl-dot)

(declaim (optimize debug))

;;; "cl-dot" goes here. Hacks and glory await!

(defclass identified ()
  ((id :reader id :initarg :id :initform nil :type (or nil id-type))))

(defclass subgraph (identified)
  ((graph.env :accessor graph.env
              :initarg :graph.env
              :initform ())
   (node.env :accessor node.env
             :initarg :node.env
             :initform ())
   (edge.env :accessor edge.env
             :initarg :edge.env
             :initform ())
   (nodes :accessor nodes
          :initarg :nodes
          :initform ())
   (contents :accessor contents
             :initform ()
             :initarg :contents
             :documentation "A list of the things defined in a subgraph.")))

;;; A graph uses --
(defclass graph (subgraph)
  ((strict :reader graph-strict :initarg :strict :initform nil :type (or nil t))))

;;; A digraph uses ->
(defclass digraph (graph)
  ())

(defclass node (identified)
  ((node.env :accessor node.env
             :initarg :node.env
             :initform ()
             :documentation "The full environment for this node.")
   (specific.env :accessor specific.env
                 :initarg :specific.env
                 :initform ()
                 :documentation "The environment settings specific for this node.")
   (edges :accessor node-edges
          :initarg :edges
          :initform ()))
  ;; (:default-initargs id (error "Nodes need IDs"))
  )

(defclass edge ()
  ((edge.env :accessor edge.env
             :initform nil
             :initarg :edge.env
             :documentation "The full environment for this edge.")
   (specific.env :accessor specific.env
                 :initarg :specific.env
                 :initform ()
                 :documentation "The environment settings specific for this edge.")
   (destination :reader destination
		:initform (error "Need a destination")
		:initarg :destination)))
