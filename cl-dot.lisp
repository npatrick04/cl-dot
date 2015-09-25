;;;; cl-dot.lisp

;;; Copyright 2015 Nick Patrick

(in-package #:cl-dot)

(declaim (optimize debug))

;;; "cl-dot" goes here. Hacks and glory await!

(defun valid-id? (str)
  (and (not (digit-char-p (elt str 0)))
       (every (lambda (char)
                (or (alphanumericp char)
                    (char= char #\_)
                    (and (>= (char-code char) 200)
                         (>= (char-code char) 377))))
              str)))

(deftype id-type ()
  `(satisfies valid-id?))

(defparameter special-ids
  '((damping . "Damping")
    (k . "K")
    (url . "URL")
    (edgeurl . "edgeURL")
    (headurl . "headURL")
    (labelurl . "labelURL")
    (tailurl . "tailURL")))

(defun symbol->id (symbol)
  "Given a symbol for an attribute, return its ID."
  (etypecase symbol
    (symbol (let ((special-case (assoc symbol special-ids)))
              (if special-case
                  (cdr special-case)
                  (string-downcase (symbol-name symbol)))))
    (string symbol)))

(defun id->symbol (id)
  "Given an attribute ID, return a symbol."
  (etypecase id
    (string (let ((special-case (rassoc id special-ids)))
              (if special-case
                  (car special-case)
                  (intern (string-upcase id) :cl-dot))))
    (symbol id)))

;;; Does this do anything?  I want a sentinel kind of marker.
(deftype anonymous ()
  '(member +anonymous+))
(declaim (type anonymous +anonymous+))
(defconstant +anonymous+ '+anonymous+)

(defclass identified ()
  ((id :reader id :initarg :id :initform nil :type (or nil anonymous id-type))))

(defclass subgraph (identified)
  ((environment :accessor environment
		:initarg :environment
		:initform (make-instance 'environment)))
  (:default-initargs :id +anonymous+))
(defmethod get-attribute ((sg subgraph) type attr)
  (get-attribute (environment sg) type attr))
(defmethod set-attribute ((sg subgraph) type attr value)
  (set-attribute (environment sg) type attr value))
(defmethod get-ancillary-attribute ((sg subgraph) type attr)
  (get-attribute (environment sg) type (make-ancillary-attribute :value attr)))
(defmethod set-ancillary-attribute ((sg subgraph) type attr value)
  (set-attribute (environment sg) type (make-ancillary-attribute :value attr) value))

;;; A graph uses --
(defclass graph (subgraph)
  ((strict :reader graph-strict :initarg :strict :initform nil :type (or nil t))
   (nodes :accessor graph-nodes :initform (make-hash-table :test 'equal))))
(defmethod initialize-instance :after ((g graph) &key &allow-other-keys)
  (set-ancillary-attribute g 'graph 'nodes (graph-nodes g))
  (set-ancillary-attribute g 'graph 'path (list (or (id g) :root)))
  (set-ancillary-attribute g 'graph 'root g))

;;; A digraph uses ->
(defclass digraph (graph)
  ())

;;; The node holds its unique attributes...and points to the inherited ones after it. 
(defclass node (identified)
  ((attributes :accessor attributes :initarg :attributes)
   (edges :accessor node-edges :initarg :edges :initform ())))

(defmethod get-attribute ((n node) type attr)
  (declare (ignore type))
  (cdr (assoc attr (attributes n) :test #'equalp)))
(defmethod set-attribute ((n node) type attr value)
  (declare (ignore type))
  (push (cons attr value) (attributes n)))
(defmethod get-ancillary-attribute ((n node) type attr)
  (get-attribute n type (make-ancillary-attribute :value attr)))
(defmethod set-ancillary-attribute ((n node) type attr value)
  (set-attribute n type (make-ancillary-attribute :value attr) value))

(defmethod initialize-instance :after ((n node) &key
                                                  (id (error "Nodes need IDs"))
                                                  (environment (error "Nodes must be created within an environment."))
                                                  &allow-other-keys)
  ;; Get the graph path, set the node attribute for path with its id
  ;; Set the root attribute to the graph being constructed
  (set-ancillary-attribute n 'node 'path
                           (cons id (get-ancillary-attribute environment 'graph 'path)))
  (set-ancillary-attribute n 'node 'root
                           (get-ancillary-attribute environment
                                                    'graph 'root))

  ;; Set the id as the key to retrieve this node
  (setf (gethash id (get-ancillary-attribute environment 'graph 'nodes)) n))

(defclass edge ()
  ((attributes :accessor attributes
	       :initform nil
	       :initarg :attributes)
   (destination :reader destination
		:initform (error "Need a destination")
		:initarg :destination)))
(defmethod get-attribute ((n edge) type attr)
  (declare (ignore type))
  (cdr (assoc attr (attributes n) :test #'equalp)))
(defmethod set-attribute ((n edge) type attr value)
  (declare (ignore type))
  (push (cons attr value) (attributes n)))
(defmethod get-ancillary-attribute ((n edge) type attr)
  (get-attribute n type (make-ancillary-attribute :value attr)))
(defmethod set-ancillary-attribute ((n edge) type attr value)
  (set-attribute n type (make-ancillary-attribute :value attr) value))

(defmethod initialize-instance :after ((e edge) &key
						  (source (error "Edges must have a source."))
						  attributes
						  (environment (error "Edges must be constructed within an environment."))
						  &allow-other-keys)
  ;; Append the edge attributes to the specific attributes for this edge.
  (setf (attributes e) (append attributes (edge-attributes environment)))

  ;; Set the root attribute to the graph being constructed.
  (set-ancillary-attribute e 'edge 'root (get-ancillary-attribute environment
								  'graph 'root))

  ;; Add to the node's adjacency list.
  (push e (node-edges source))

  (unless (typep (get-ancillary-attribute environment 'graph 'root) 'digraph)
    ;; It's an undirected graph...add an edge in the other direction
    (push (make-instance 'edge
                         :environment environment
                         :destination source
                         :attributes attributes)
          (node-edges (destination e)))))

(defun get-node-by-id (graph node-id)
  (gethash node-id (get-ancillary-attribute graph
					    'graph
					    'nodes)))

;; (let* ((g (make-instance 'digraph))
;;        (n1 (make-instance 'node :id "foo" :environment (environment g)))
;;        (n2 (make-instance 'node :id "bar" :environment (environment g)))
;;        (e (make-instance 'edge :source n1 :destination n2 :environment (environment g))))
;;   (declare (ignorable e))
;;   g)

