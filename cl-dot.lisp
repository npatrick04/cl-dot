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

(defclass identified ()
  ((id :reader id :initarg :id :initform nil :type (or nil id-type))))

(defclass attributed ()
  ((attributes :accessor attributes :initarg :attributes :initform nil)))

(defun print-alist (alist stream &optional (bracket t))
  (when alist
    (format stream "~:[~;[~]~{~A~^, ~}~:[~;]~]"
            bracket
            (loop for (attr . value) in alist
               collect (format nil "~{~A=\"~A\"~}"
                               (mapcar #'symbol->id (list attr value))))
            bracket)))

(defun normalize-statement (statement)
  (etypecase statement
    (string
     (let ((arrow (search "->" statement)))
       (if arrow
           (make-instance 'edge :nodes (list (normalize-statement
                                              (subseq statement 0 arrow))
                                             (normalize-statement
                                              (subseq statement (+ 2 arrow)))))
           (make-instance 'node :id (string-downcase statement)))))
    (cons
     (print-alist (list statement) nil nil))
    (symbol
     (normalize-statement (symbol-name statement)))))

(defclass subgraph (identified)
  ((node-attrs :accessor node-attrs :initarg :node-attrs :initform nil)
   (edge-attrs :accessor edge-attrs :initarg :edge-attrs :initform nil)
   (statements :accessor statements :initarg :statements :initform nil)))
(defmethod initialize-instance :after ((object subgraph) &key &allow-other-keys)
  (setf (statements object) (mapcar #'normalize-statement (statements object))))

(defmethod print-object ((object subgraph) stream)
  (format stream
          "~:[~;strict ~]~:[~:[subgraph~;graph~]~;~*digraph~] ~:[~;~:*~A ~]~%~
           {~%~
             ~:[~;  ~:*node ~A;~%~]~
             ~:[~;  ~:*edge ~A;~%~]~
             ~{  ~A;~%~}~
           }~%"
          (graph-strict object)
          (typep object 'digraph)
          (typep object 'graph)
          (when (id object) (string-downcase (id object)))
          (when (node-attrs object) (print-alist (node-attrs object) nil))
          (when (edge-attrs object) (print-alist (edge-attrs object) nil))
          (statements object)))

;;; A graph uses --
(defclass graph (subgraph)
  ((strict :reader graph-strict :initarg :strict :initform nil :type (or nil t))))

;;; A digraph uses ->
(defclass digraph (graph)
  ())

(defclass node (identified attributed)
  ())
;; (defmethod initialize-instance :after ((object node) &key &allow-other-keys)
;;; Perhaps set a reference to a hash of all nodes...
;;   (setf (get (id->symbol (id object)) 'node) object))

(defmethod print-object :after ((object attributed) stream)
  (print-alist (attributes object) stream))

(defmethod print-object ((object node) stream)
  (format stream "~A" (string-downcase (id object))))

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
          (string-downcase (id (car (nodes object))))
          (string-downcase (id (cadr (nodes object)))))) ;TODO: add attributes

(defun dot-read (str)
  (string-trim-left " \n" str))

(defun read-graph (g)
  )
