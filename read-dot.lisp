;;; Copyright 2015 Nick Patrick

(in-package #:cl-dot)

(declaim (optimize debug))

(declaim (special *environment*))
(defvar *environment* nil)

;;; Utility rules

(defrule whitespace (+ (or #\space #\tab #\newline))
  (:constant nil))

(defrule id-type (and (? whitespace)
		      (? #\") (+ (alphanumericp character))  (? #\"))
  (:destructure (w ql s qr)
		(declare (ignore w ql qr))
		(text s)))

;;; Dot Rules

(defrule strict (and (? whitespace) "strict")
  (:constant :strict))
(defrule graph-type (and (? whitespace) "graph")
  (:constant :graph))
(defrule digraph-type (and (? whitespace) "digraph")
  (:constant :digraph))

(defrule graph-spec (and (? strict)
			 (or graph-type digraph-type)
			 (? id-type) (? whitespace))
  (:destructure (strict gtype id? w1 &bounds start end)
		(declare (ignore w1))
		;; Now we know the type of graph we need to create.
		;; Get it started by making that graph.  
		(let ((graph (make-instance (ecase gtype
					      (:graph 'graph)
					      (:digraph 'digraph))
					    :strict (eq strict :strict)
					    :id id?
					    :environment *environment*)))
		  ;; Now we read statements until we find the
		  ;; end of the graph.
		  ;; (do ((statement (parse ))))
		  (format t "Parsing graph spec, bounds {~d -> ~d}~%"
			  start end)
		  graph)))

(defrule scope (and (and #\{ (? whitespace))
		    stmt-list (? whitespace)
		    (and #\} (? whitespace)))
  (:function second)
  (:around ()
	   (let ((*environment* (make-extended-environment
				 *environment*)))
	     (format t "Extend environment~%")
	     (call-transform))))

(defrule graph-structure (and graph-spec scope)
  (:function first)
  (:around ()
	   (let ((*environment* (make-instance 'environment)))
	     (format t "Make new environment~%")
	     (call-transform))))

(defrule stmt-list (and stmt (? #\;) (? stmt-list))
  (:destructure (car semi cdr)
		(declare (ignore semi))
		(format t "{~D, ~D}~%" car cdr)
		(cons car cdr)))

(defrule stmt (and (? whitespace)
		   (or attr-stmt
		       ;edge-stmt
		       node-stmt
		       ;(and id-type (? whitespace) #\= (? whitespace) id-type)
		       ;; subgraph
		       ))
  (:destructure (w stmt)
		(declare (ignore w))
		(print stmt)
		stmt))

(defrule node-attr "node" (:constant 'node))
(defrule graph-attr "graph" (:constant 'graph))
(defrule edge-attr "edge" (:constant 'edge))

(defrule attr-stmt (and (or node-attr
			    graph-attr
			    edge-attr)
			attr-list)
  (:destructure (attr-type attrs)
		(ecase attr-type
		  (node (augment-environment *environment*
					     :new-node-attributes attrs))
		  (edge (augment-environment *environment*
					     :new-edge-attributes attrs)) 
		  (graph (augment-environment *environment*
					     :new-graph-attributes
					     attrs)))))
(defrule attr-list (and #\[ (? a-list) #\] (? attr-list))
  (:destructure (bl alist br rest)
		(declare (ignore bl br))
		(append alist rest)))
(defrule a-list (and id-type
		     (? whitespace) #\=
		     id-type
		     (? whitespace) (? (or #\; #\,)) (? a-list))
  (:destructure (id1 w1 eq id2 w2 term rest)
		(declare (ignore w1 w2 eq term))
		(cons (cons id1 id2) rest)))
(defrule edge-stmt (and (or node-id subgraph) edge-rhs (? attr-list))
  (:destructure (lhs rhs attrs)
		(format t "edge ~a -> ~A [~{~A~^,~}]~%" lhs rhs attrs)
		(let ((g (get-ancillary-attribute *environment*
						  'graph
						  'root)))
		  (let ((lhs-node (get-node-by-id g lhs)))
		    (dolist (rhs rhs)
		      
		      (push (make-instance 'edge
					   :environment *environment*
					   :source lhs
					   :destination )))))
    (list lhs rhs attrs)))
(defrule edge-op (or "--" "->")
  (:text t))
(defrule edge-rhs (and edge-op (or node-id subgraph) (? edge-rhs))
  (:destructure (op rhs rest)
		(declare (ignore op))
    ;; TODO: Validate op
    (cons rhs rest)))
(defrule node-stmt (and node-id (? attr-list))
  (:destructure (id attrs)
		(format t "node ~A~%" id)
    (make-instance 'node
		   :id id
		   :environment *environment*
		   :attributes attrs)))

(defrule node-id (and id-type (? port))
  (:destructure (id port)
		(declare (ignore port))
    ;; TODO: ports
    id))
(defrule port (or (and #\: id-type (? (and #\: compass-pt)))
		  (and #\: compass-pt)))
(defrule subgraph (and (? (and "subgraph" (? id-type))) #\{ stmt-list (? whitespace) #\}))
(defrule compass-pt (or "ne" "nw" "n" "se" "sw" "s" "e" "w" "c" #\_)
  (:text t))

(parse 'strict "strict")
(parse 'graph-structure "strict digraph bar { baz; }")
(parse 'graph-structure "strict digraph bar { baz; }" :junk-allowed t)
(parse 'graph-structure "digraph bar { baz[shape=box]; }" :junk-allowed t)
(parse 'graph-structure "digraph
{
  foo;
  bar;
  baz[shape=box];
}")
