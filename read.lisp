;;; Copyright 2015 Nick Patrick

(in-package #:cl-dot)

(declaim (optimize debug))

(declaim (special *environment*))
(defvar *environment* nil)

;;; Utility rules

(defrule whitespace (+ (or #\space #\tab))
  (:text t))
(defrule whitespace/nl (+ (or #\space #\tab #\newline))
  (:text t))

(defrule string-type (and (? whitespace/nl) (+ (or (alphanumericp character) #\_)))
  (:destructure (w s)
		(declare (ignore w))
		(text s)))

(defrule quoted-string-type (and (? whitespace/nl)
				 (? #\")
				 (+ (or whitespace/nl
					#\_
					(alphanumericp character)))
				 (? #\"))
  (:destructure (w ql s qr)
		(declare (ignore w ql qr))
		(text s)))

(defrule html-string (and #\< (+ (or whitespace/nl (alphanumericp character))) #\>)
  (:text t))

(defrule digit (character-ranges (#\0 #\9)))

(defrule integer/str (+ digit)
  (:text t))

(defrule integer integer/str
  (:lambda (int-str)
	   (parse-integer int-str)))

(defrule numeral/str (and (? #\-)
			  (or (and #\. integer/str)
			      (and integer/str (? (and #\. (* digit))))))
  (:text t))

(defrule numeral numeral/str
  (:function read-from-string))



(defrule id-type (or numeral string-type quoted-string-type html-string))

;;; Dot Rules

(defrule strict (and (? whitespace/nl) "strict")
  (:constant :strict))
(defrule graph-type (and (? whitespace/nl) "graph")
  (:constant :graph))
(defrule digraph-type (and (? whitespace/nl) "digraph")
  (:constant :digraph))

(defrule graph-spec (and (? strict)
			 (or graph-type digraph-type)
			 (? id-type)
			 (? whitespace/nl))
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

(defrule scope (and (and #\{ (? whitespace/nl))
		    stmt-list (? whitespace/nl)
		    (and #\} (? whitespace/nl)))
  (:destructure (open stmts ws close)
		(declare (ignore open ws close))
		(format t "Returning scope: ~A~%" stmts)
		stmts)
  (:around ()
	   (format t "Scope, Extend environment ~A~%" *environment*)
	   (let ((*environment* (make-extended-environment
				 *environment*)))
	     (format t "      Extend environment now ~A~%" *environment*)
	     (call-transform)
	     (format t "Scope, done extending ~A~%" *environment*))
	   (format t "       Environment back to ~A~%" *environment*)))

(defrule graph-structure (and graph-spec scope)
  (:destructure (graph contents)
		(list graph contents))
  (:around ()
	   (let ((*environment* (make-instance 'environment)))
	     (format t "Make new environment~%")
	     (call-transform))))

(defrule stmt-list (and stmt (? #\;) (? stmt-list))
  (:destructure (car semi cdr)
		(declare (ignore semi))
		(format t "(~{~A ~})~%" (cons car cdr))
		(when car		  
		  (cons car cdr))))
(defrule stmt (and (? whitespace/nl)
		   (?
		    (or attr-stmt
			subgraph
					;edge-stmt
			node-stmt
					;(and id-type (? whitespace/nl) #\= (? whitespace/nl) id-type)
			
			)))
  (:destructure (w stmt)
		(declare (ignore w))
;;		(print stmt)
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
		     (? whitespace/nl) #\=
		     id-type
		     (? whitespace/nl) (? (or #\; #\,)) (? a-list))
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
		      
		      ;; (push (make-instance 'edge
		      ;; 			   :environment *environment*
		      ;; 			   :source lhs
		      ;; 			   :destination ))
		      )))
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
		(let ((node (make-instance 'node
					   :id id
					   :environment *environment*
					   :attributes attrs)))
		  (format t "node ~A, ~D attributes~%" id
			  (length (attributes node)))
		  node)))

(defrule node-id (and id-type (? port))
  (:destructure (id port)
		(declare (ignore port))
    ;; TODO: ports
    id))
(defrule port (or (and #\: id-type (? (and #\: compass-pt)))
		  (and #\: compass-pt)))
(defrule subgraph-identifier "subgraph" (:constant 'subgraph))

(defrule subgraph-spec (and (? (and subgraph-identifier (? id-type)
			       (? whitespace/nl))))
  (:destructure ((sg id wt))
		(declare (ignore sg wt))
		(format t "Create subgraph ~A~%" id)
		(make-instance 'subgraph
			       :id id
			       :environment (setf *environment*
						  (make-extended-environment *environment*))))
  ;; (:around ()
  ;; 	   (let ((*environment* (make-extended-environment
  ;; 				 *environment*)))
  ;; 	     (format t "subgraph around, extending env~%")
  ;; 	     (call-transform)))
  )
(defrule subgraph (and subgraph-spec scope)
  (:destructure (spec scope)
                (format t "Subgraph spec: ~A~%Scope: ~A~%" spec scope)
                scope))

(defrule compass-pt (or "ne" "nw" "n" "se" "sw" "s" "e" "w" "c" #\_)
  (:text t))

(parse 'attr-list "[shape=diamond,style=blah,number=3.14,foo=bar]")

(parse 'strict "strict")
(parse 'graph-structure "strict digraph bar { baz; }")
(parse 'graph-structure "strict digraph bar { baz; }" :junk-allowed t)
(parse 'graph-structure "digraph bar { baz[shape=box]; }" :junk-allowed t)

(parse 'graph-structure "digraph
{
  node[shape=diamond;style=dashed];
  foo;
  bar[shape=box];
  subgraph cluster_qux
  {
    node[fill=yes];
    qux[label=\"qux\"];
  }
  baz;
}")
(parse 'graph-structure "digraph
{
  node[shape=diamond;style=dashed];
  foo;
  subgraph blah { node[style=\"solid\"]; bar; }
  baz[shape=box];
}")
