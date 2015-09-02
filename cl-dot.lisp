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
(defmethod get-attribute ((sg subgraph) type attr)
  (get-attribute (environment sg) type attr))
(defmethod set-attribute ((sg subgraph) type attr value)
  (set-attribute (environment sg) type attr value))
(defmethod get-ancillary-attribute ((sg subgraph) type attr)
  (get-attribute (environment sg) type (make-ancillary-attribute :value attr)))
(defmethod set-ancillary-attribute ((sg subgraph) type attr value)
  (set-attribute (environment sg) type (make-ancillary-attribute :value attr) value))

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

(defmethod initialize-instance :around ((n node) &key
					id
					attributes
					environment
					&allow-other-keys)
  (let ((new-attributes (list (cons 'path (cons id
						(get-ancillary-attribute environment
									 'graph 'path)))
			      (cons 'root (get-ancillary-attribute environment
								   'graph 'root))))
	(node-set (get-ancillary-attribute environment 'graph 'nodes)))
    (setf (gethash id node-set)
	  (call-next-method n
			    :attributes (append attributes
						new-attributes
						(node-attributes environment))))))

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
						  source
						  attributes
						  environment
						  &allow-other-keys)
  (setf (attributes e) (append attributes (edge-attributes environment)))
  (set-ancillary-attribute e 'edge 'root (get-ancillary-attribute environment
								  'graph 'root))
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

;; (let* ((g (make-instance 'digraph))
;;        (n1 (make-instance 'node :id "foo" :environment (environment g)))
;;        (n2 (make-instance 'node :id "bar" :environment (environment g)))
;;        (e (make-instance 'edge :source n1 :destination n2 :environment (environment g))))
;;   (declare (ignorable e))
;;   g)

