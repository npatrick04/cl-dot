;;;; cl-dot.lisp

;;; Copyright 2015 Nick Patrick

(in-package #:cl-dot)

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



(defgeneric get-attribute (place type attr))
(defgeneric set-attribute (place type attr value))

(defmethod get-attribute ((e environment) type attr)
  (cdr (assoc attr (ecase type
		     (graph (graph-attributes e))
		     (node (node-attributes e))
		     (edge (edge-attributes e)))
	      :test #'equalp)))
(defmethod set-attribute ((e environment) type attr value)
  (let ((new-attributes
	 (cons (cons attr value)
	       (ecase type
		 (graph (graph-attributes e))
		 (node (node-attributes e))
		 (edge (edge-attributes e))))))
    (case type
      (graph (setf (graph-attributes e) new-attributes))
      (node (setf (node-attributes e) new-attributes))
      (edge (setf (edge-attributes e) new-attributes))))
  value)

;; An attribute that is for the use by cl-dot
(defstruct ancillary-attribute 
  (value))
(defgeneric get-ancillary-attribute (place type attr))
(defgeneric set-ancillary-attribute (place type attr value))

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

;; (defun print-alist (alist stream &optional (bracket t))
;;   (when alist
;;     (format stream "~:[~;[~]~{~A~^, ~}~:[~;]~]"
;;             bracket
;;             (loop for (attr . value) in alist
;;                collect (format nil "~{~A=\"~A\"~}"
;;                                (mapcar #'symbol->id (list attr value))))
;;             bracket)))

;; (defun normalize-statement (statement)
;;   (etypecase statement
;;     (string
;;      (let ((arrow (search "->" statement)))
;;        (if arrow
;;            (make-instance 'edge :nodes (list (normalize-statement
;;                                               (subseq statement 0 arrow))
;;                                              (normalize-statement
;;                                               (subseq statement (+ 2 arrow)))))
;;            (make-instance 'node :id (string-downcase statement)))))
;;     (cons
;;      (print-alist (list statement) nil nil))
;;     (symbol
;;      (normalize-statement (symbol-name statement)))))

(defclass identified ()
  ((id :reader id :initarg :id :initform nil :type (or nil id-type))))

(defclass subgraph (identified)
  ((environment :accessor environment
		:initarg :environment
		:initform (make-instance 'environment))))
(defmethod get-ancillary-attribute ((sg subgraph) type attr)
  (get-attribute (environment sg) type (make-ancillary-attribute :value attr)))
(defmethod set-ancillary-attribute ((sg subgraph) type attr value)
  (set-attribute (environment sg) type (make-ancillary-attribute :value attr) value))
(defmethod get-attribute ((sg subgraph) type attr)
  (get-attribute (environment sg) type attr))
(defmethod set-attribute ((sg subgraph) type attr value)
  (set-attribute (environment sg) type attr value))

;; (defmethod initialize-instance :after ((object subgraph) &key &allow-other-keys)
;;   (setf (subgraph-statements object)
;; 	(mapcar #'normalize-statement (subgraph-statements object))))
;; (defmethod initialize-instance :around ((object subgraph) &key
;; 							    environment
;; 							    statements
;; 							    &allow-other-keys)
;;   (let ((normalized-statements (mapcar #'normalize-statement statements)))
;;     (if environment
;; 	(call-next-method object
;; 			  :environment (make-extended-environment environment)
;; 			  :statements normalized-statements)
;; 	(call-next-method object
;; 			  :statements normalized-statements))))


;; (defmethod print-object ((object subgraph) stream)
;;   (print-unreadable-object (object stream :type t :identity t))
;;   ;; (format stream
;;   ;;         "~:[~;strict ~]~:[~:[subgraph~;graph~]~;~*digraph~] ~:[~;~:*~A ~]~%~
;;   ;;          {~%~
;;   ;;            ~:[~;  ~:*node ~A;~%~]~
;;   ;;            ~:[~;  ~:*edge ~A;~%~]~
;;   ;;            ~{  ~A;~%~}~
;;   ;;          }~%"
;;   ;;         (graph-strict object)
;;   ;;         (typep object 'digraph)
;;   ;;         (typep object 'graph)
;;   ;;         (when (id object) (string-downcase (id object)))
;;   ;;         (when (node-attrs object) (print-alist (node-attrs object) nil))
;;   ;;         (when (edge-attrs object) (print-alist (edge-attrs object) nil))
;;   ;;         (statements object))
;;   )

;;; A graph uses --
(defclass graph (subgraph)
  ((strict :reader graph-strict :initarg :strict :initform nil :type (or nil t))
   (nodes :accessor graph-nodes :initform (make-hash-table :test 'equal))))
(defmethod initialize-instance :after ((g graph) &key &allow-other-keys)
  (set-ancillary-attribute g 'graph 'nodes (graph-nodes g))
  (set-ancillary-attribute g 'graph 'path '(:root))
  (set-ancillary-attribute g 'graph 'root g))

;;; A digraph uses ->
(defclass digraph (graph)
  ())

;;; The node holds its unique attributes...and points to the inherited ones after it. 
(defclass node (identified)
  ((attributes :accessor attributes :initarg :attributes)
   (edges :accessor node-edges :initarg :edges :initform ())))

(defmethod get-attribute ((n node) type attr)
  (unless (eq type 'node) (error "Type must be 'node"))
  (cdr (assoc attr (attributes n) :test #'equalp)))
(defmethod set-attribute ((n node) type attr value)
  (unless (eq type 'node) (error "Type must be 'node"))
  (push (cons attr value) (attributes n)))
(defmethod get-ancillary-attribute ((n node) type attr)
  (get-attribute n type (make-ancillary-attribute :value attr)))
(defmethod set-ancillary-attribute ((n node) type attr value)
  (set-attribute n type (make-ancillary-attribute :value attr) value))

(defmethod initialize-instance :after ((n node) &key
						  attributes
						  environment
						  &allow-other-keys)
  (when environment
    (setf (attributes n) (append attributes (node-attributes environment)))
    (set-ancillary-attribute n 'node 'path
			     (cons (id n) (get-ancillary-attribute environment
								   'graph 'path)))
    (set-ancillary-attribute n 'node 'root (get-ancillary-attribute environment
								    'graph 'root))
    (let ((node-set (get-ancillary-attribute environment 'graph 'nodes)))
      (setf (gethash (id n) node-set) n))))

;; (defmethod initialize-instance :after ((object node) &key &allow-other-keys)
;;; Perhaps set a reference to a hash of all nodes...
;;   (setf (get (id->symbol (id object)) 'node) object))

;; (defmethod print-object :after ((object environment) stream)
;;   (print-alist (attributes object) stream))

;; (defmethod print-object ((object node) stream)
;;   (format stream "~A" (string-downcase (id object))))

(defun node-or-subgraph? (element)
  (or (typep element 'node)
      (typep element 'subgraph)))

(defun node-or-subgraph-list? (element)
  (format t "check node-or-subgraph-list?~%")
  (every #'node-or-subgraph? element))

(deftype node-or-subgraph-list ()
  `(satisfies node-or-subgraph-list?))

(defclass edge ()
  ((attributes :accessor attributes
	       :initform nil
	       :initarg :attributes)
   (destination :reader destination
		:initform (error "Need a destination")
		:initarg :destination)))
(defmethod get-attribute ((n edge) type attr)
  (unless (eq type 'edge) (error "Type must be 'edge"))
  (cdr (assoc attr (attributes n) :test #'equalp)))
(defmethod set-attribute ((n edge) type attr value)
  (unless (eq type 'edge) (error "Type must be 'edge"))
  (push (cons attr value) (attributes n)))
(defmethod get-ancillary-attribute ((n edge) type attr)
  (get-attribute n type (make-ancillary-attribute :value attr)))
(defmethod set-ancillary-attribute ((n edge) type attr value)
  (set-attribute n type (make-ancillary-attribute :value attr) value))
(defmethod initialize-instance :after ((e edge) &key
						  source
						  attributes
						  environment
						  &allow-other-keys)
  (declare (optimize debug))
  (when environment
    (setf (attributes e) (append attributes (edge-attributes environment)))
    (set-ancillary-attribute e 'edge 'root (get-ancillary-attribute environment
								    'graph 'root))
    ;; (unless (typep (get-ancillary-attribute environment 'graph 'root) 'digraph)
    ;;   ;; It's an undirected graph...add an edge in the other direction
    ;;   (push (make-instance 'edge
    ;; 			   :environment environment
    ;; 			   :destination source
    ;; 			   :attributes attributes)
    ;; 	    (node-edges (destination e))))
    )
  (when source
    (push e (node-edges source))
    (when environment
      (unless (typep (get-ancillary-attribute environment 'graph 'root) 'digraph)
	;; It's an undirected graph...add an edge in the other direction
	(push (make-instance 'edge
			     :environment environment
			     :destination source
			     :attributes attributes)
	      (node-edges (destination e)))))))
;; (defmethod print-object ((object edge) stream)
;;   (format stream "~A -> ~A"
;;           (string-downcase (id (car (nodes object))))
;;           (string-downcase (id (cadr (nodes object))))))
					;TODO: add attributes

(defun get-node-by-id (graph node-id)
  (gethash node-id (get-ancillary-attribute graph
					    'graph
					    'nodes)))

(let* ((g (make-instance 'digraph))
       (n1 (make-instance 'node :id "foo" :environment (environment g)))
       (n2 (make-instance 'node :id "bar" :environment (environment g)))
       (e (make-instance 'edge :source n1 :destination n2 :environment (environment g))))
  (declare (ignorable e))
  g)

