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

(defun symbol->attr (symbol)
  "Given a symbol for an attribute, return its ID."
  (let ((special-case (assoc symbol special-ids)))
    (if special-case
        (cdr special-case)
        (string-downcase (symbol-name symbol)))))

(defun id->symbol (id)
  "Given an attribute ID, return a symbol."
  (let ((special-case (rassoc id special-ids)))
    (if special-case
        (car special-case)
        (intern (string-upcase id) :cl-dot))))

(defclass identified ()
  ((id :reader id :initarg :id :initform nil :type (or nil id-type))))

(defclass attributed ()
  ((attributes :accessor attributes :initarg :attributes)))

(defun normalize-statement (statement)
  (etypecase statement
    (string
     (let ((arrow (search "->" statement)))
       (if arrow
           (make-instance 'edge :nodes (list (normalize-statement
                                              (subseq statement 0 arrow))
                                             (normalize-statement
                                              (subseq statement (+ 2 arrow)))))
           (make-instance 'node :id statement))))
    (symbol
     (normalize-statement (symbol-name statement)))))

(defclass subgraph (identified)
  ((node-attrs :accessor node-attrs :initarg :node-attrs)
   (edge-attrs :accessor node-attrs :initarg :node-attrs)
   (statements :accessor statements :initarg :statements)))
(defmethod :after initialize-instance ((object subgraph) &key &allow-other-keys)
  (mapc #'normalize-statement statements))

(defmethod print-object ((object subgraph) stream)
  (format stream
          "~:[~;strict ~]~:[~:[subgraph~;graph~]~;~*digraph~] ~:[~;~:*~A ~]~%~
           {~%~{  ~A;~%~}}~%"
          (graph-strict object)
          (typep object 'digraph)
          (typep object 'graph)
          (when (id object) (string-downcase (id object)))
          (statements object)))

;;; A graph uses --
(defclass graph (subgraph)
  ((strict :reader graph-strict :initarg :strict :initform nil :type (or nil t))
   (identifiers :accessor identifiers :initform (make-hash-table))))

;;; A digraph uses ->
(defclass digraph (graph)
  ())

(defclass node (identified attributed)
  ())

(defmethod print-object :after ((object attributed) stream)
  (format stream "[~{~A~^, ~}]"
          (loop for (attr . value) in (attributes object)
               collect (format nil "~A=\"~A\"" attr value))))

(defmethod print-object ((object node) stream)
  (format stream "~A"
          (id object)))    ;TODO: add attributes

(defun node-or-subgraph? (element)
  (or (typep element 'node)
      (typep element 'subgraph)))

(defun node-or-subgraph-list? (element)
  (format t "check node-or-subgraph-list?~%")
  (every #'node-or-subgraph? element))

(deftype node-or-subgraph-list ()
  `(satisfies node-or-subgraph-list?))

(defclass edge (attributed)
  ((nodes :accessor nodes :initarg :nodes :type node-or-subgraph-list)))

(defmethod print-object ((object edge) stream)
  (format stream "~A -> ~A"
          (id (car (nodes object)))
          (id (cadr (nodes object)))))    ;TODO: add attributes

(defun dot-read (str)
  (string-trim-left " \n" str))

(defun read-graph (g)
  )
