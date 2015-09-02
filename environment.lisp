(in-package :cl-dot)

(defparameter *default-graph-attributes* ()) 
(defparameter *default-node-attributes* ()) 
(defparameter *default-edge-attributes* ()) 

;; An environment that can be extended.  
(defclass environment ()
  ((graph-attributes :accessor graph-attributes
		     :initarg :graph-attributes
		     :initform *default-graph-attributes*)
   (node-attributes  :accessor node-attributes
		     :initarg :node-attributes
		     :initform *default-node-attributes*)
   (edge-attributes  :accessor edge-attributes
		     :initarg :edge-attributes
		     :initform *default-edge-attributes*)))

;;; A user-specified type of attribute
(defgeneric get-attribute (place type attr)
  (:documentation "Given an object with attributes, and
a type of 'graph, 'node, or 'edge, retrieve the
attribute stored by the key attr."))
(defgeneric set-attribute (place type attr value)
  (:documentation "Given an object with attributes, and
a type of 'graph, 'node, or 'edge, store value in the
attribute stored by the key attr."))

;;; An attribute that is for the use by cl-dot
(defstruct ancillary-attribute (value))
(defgeneric get-ancillary-attribute (place type attr)
  (:documentation "Like get-attribute, except the key
is converted into an ancillary-attribute type."))
(defgeneric set-ancillary-attribute (place type attr value)
  (:documentation "Like set-attribute, except the key
is converted into an ancillary-attribute type."))

(defmethod get-attribute ((e environment) type attr)
  (cdr (assoc attr (ecase type
		     (graph (graph-attributes e))
		     (node (node-attributes e))
		     (edge (edge-attributes e)))
	      :test #'equalp)))

(defmethod set-attribute ((e environment) type attr value)
  (ecase type
    (graph (push (cons attr value) (graph-attributes e)))
    (node (push (cons attr value) (node-attributes e)))
    (edge (push (cons attr value) (edge-attributes e))))
  value)

(defmethod get-ancillary-attribute ((e environment) type attr)
  (get-attribute e type (make-ancillary-attribute :value attr)))
(defmethod set-ancillary-attribute ((e environment) type attr value)
  (set-attribute e type (make-ancillary-attribute :value attr) value))

(defun augment-attributes (old-attributes new-attributes)
  (append new-attributes old-attributes))

(defun make-extended-environment (old-environment &key
						    new-graph-attributes
						    new-node-attributes
						    new-edge-attributes)
  "Given new attributes, return a new environment, now augmented."
  (make-instance 'environment
		 :graph-attributes (augment-attributes
				    (graph-attributes old-environment)
				    new-graph-attributes)
		 :node-attributes (augment-attributes
				   (node-attributes old-environment)
				   new-node-attributes)
		 :edge-attributes (augment-attributes
				   (edge-attributes old-environment)
				   new-edge-attributes)))
(defun augment-environment (old-environment &key
					      new-graph-attributes
					      new-node-attributes
					      new-edge-attributes)
  "Prepend new attributes to the environment."
  (setf (graph-attributes old-environment) (augment-attributes
					    (graph-attributes old-environment)
					    new-graph-attributes)
	(node-attributes old-environment) (augment-attributes
					   (node-attributes old-environment)
					   new-node-attributes)
	(edge-attributes old-environment) (augment-attributes
					   (edge-attributes old-environment)
					   new-edge-attributes)))
