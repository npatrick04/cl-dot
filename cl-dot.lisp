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

(defgeneric connector-style (graph)
  (:documentation "The symbol for the connector of this style graph."))
(defmethod connector-style (not-a-graph)
  (error "This is not a graph! ~A" not-a-graph))
(defmethod connector-style ((graph graph))
  '--)
(defmethod connector-style ((graph digraph))
  '->)

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
          :initform ())))

(defun lookup-node-attribute (id node &key (test #'eq))
  "Get the node attribute, searching the node's specific environment
first, then the node's creation environment."
  (handler-case
      (lookup id (specific.env node) :test test)
    (lookup-failure (c)
      (declare (ignore c))
      (lookup id (node.env node) :test test))))

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

(defun make-digraph-edge (source destination specific-attributes edge.env)
  (let ((edge (make-instance 'edge
                             :destination destination
                             :specific.env specific-attributes
                             :edge.env edge.env)))
    (push edge (node-edges source))
    edge))

(defun make-graph-edge (source destination specific-attributes edge.env)
  (list (make-digraph-edge source destination specific-attributes edge.env)
        (make-digraph-edge destination source specific-attributes edge.env)))

(defun lookup-edge-attribute (id edge &key (test #'eq))
  "Get the edge attribute, searching the edge's specific environment
first, then the edge's creation environment."
  (handler-case
      (lookup id (specific.env edge) :test test)
    (lookup-failure (c)
      (declare (ignore c))
      (lookup id (edge.env edge) :test test))))

