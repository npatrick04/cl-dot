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
(defmethod connector-style ((sg subgraph))
  (connector-style (lookup 'graph (graph.env sg))))

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

(defun node-attributes (node)
  (append (specific.env node)
          (node.env node)))

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

(defun make-digraph-edge (source destination edge.env)
  (let ((edge (make-instance 'edge
                             :destination destination
                             :edge.env edge.env)))
    (push edge (node-edges source))
    edge))

(defun make-graph-edge (source destination edge.env)
  (list (make-digraph-edge source destination edge.env)
        (make-digraph-edge destination source edge.env)))

(defun lookup-edge-attribute (id edge &key (test #'eq))
  "Get the edge attribute, searching the edge's specific environment
first, then the edge's creation environment."
  (handler-case
      (lookup id (specific.env edge) :test test)
    (lookup-failure (c)
      (declare (ignore c))
      (lookup id (edge.env edge) :test test))))

(defun lookup-or-create-node (exp graph.env)
  (let ((id (lispify-symbol exp))
        (nodes (lookup 'nodes graph.env)))
    (handler-case
        (lookup id nodes)
      (lookup-failure (c)
        (declare (ignore c))
        (let ((node (make-instance 'node
                                   :id exp
                                   :node.env (node.env (lookup
                                                        'subgraph
                                                        graph.env)))))
          (update 'nodes graph.env (extend nodes id node))
          node)))))

(defun make-edges (subgraph source dest)
  (let ((sources (if (consp source)
                     source
                     (list source)))
        (dests (if (consp dest)
                   dest
                   (list dest)))
        (graph (lookup 'graph (graph.env subgraph)))
        result)
    (loop for source in sources
          do (loop for dest in dests
                   do (push (typecase graph
                              (digraph (make-digraph-edge source dest (edge.env subgraph)))
                              (graph   (make-graph-edge source dest (edge.env subgraph))))
                            result))
          finally (return result))))

(defun edge-spec-nodes (edge)
  (let (result)
    (loop for lhs in edge by #'cddr
          while lhs
          do (typecase lhs
               (node (pushnew lhs result))
               (subgraph
                (setf result (union (subgraph-nodes lhs)
                                    result)))
               (t (error "Edge must contain nodes or subgraphs")))
          finally (return result))))

(defun subgraph-nodes (subgraph)
    (do ((result ())
       (to-be-checked (contents subgraph) (cdr to-be-checked)))
      ((null to-be-checked) result)
    (cond
      ((typep (car to-be-checked) 'node)
       (push (car to-be-checked) result))
      ((typep (car to-be-checked) 'subgraph)
       (setf result (union (subgraph-nodes (car to-be-checked))
                           result)))
      ((eq (cadar to-be-checked)
           (connector-style (lookup 'graph (graph.env subgraph))))
       (setf result (union (edge-spec-nodes (car to-be-checked))
                           result))))))

