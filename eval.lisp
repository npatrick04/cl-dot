(in-package :cl-dot)
(declaim (optimize debug))

(defun evaluate (e n.env e.env g.env)
  (if (atom e)
      (cond ((symbolp e) e)
            ((or (numberp e) (stringp e) (characterp e) (vectorp e))
             e)
            
            (t (error "Cannot evaluate: ~A" e)))
      (if (symbolp (car e))
          (case (car e)
            (subgraph
             (let ((name (when (symbolp (cadr e))
                           (cadr e))))
               (let ((subgraph (make-instance 'subgraph
                                              :id name
                                              :node.env  n.env
                                              :edge.env  e.env)))
                 (let ((new-graph-env (extend g.env 'subgraph subgraph)))
                   (setf
                    (graph.env subgraph) new-graph-env
                    (contents subgraph) (evlis
                                         (if (symbolp (cadr e))
                                             (caddr e)
                                             (cadr e))
                                         n.env
                                         e.env
                                         new-graph-env))
                   subgraph))))
            ;; (node ((shape . bar))
            (node
             (if (consp (cadr e))
                 ;; Looks like a node environment extension...I guess.
                 (setf (node.env (lookup 'subgraph g.env))
                       (append (cadr e) n.env))
                 
                 (error "The node statement requires a list of parameters: ~A"
                        (cadr e))))

            (edge
             (if (consp (cadr e))
                 ;; Looks like a edge environment extension...I guess.
                 (setf (edge.env (lookup 'subgraph g.env))
                       (append (cadr e) e.env))
                 
                 (error "The edge statement requires a list of parameters: ~A"
                        (cadr e))))

            (t
             (if (extend g.env (car e) (caddr e))
                 (let* ((nodes (lookup 'nodes g.env))
                        (node (lookup-or-create-node (car e) nodes n.env)))
                   (format t "nodes: ~A~%node: ~A~%" nodes node)
                   (when (consp (cadr e))
                     (setf (specific.env node) (cadr e)))
                   (update 'nodes g.env (extend nodes (car e) node))
                   node))))
          ;; Not a symbolp...anonymous subgraph is the thing.
          ;; TODO, anonymous subgraph
          )))

(defun evlis (e n.env e.env g.env)
  (declare (ignore n.env e.env))
  (format t "~&evlis ~A~%  n.env: ~A~%  e.env: ~A~%  g.env: ~A~%" e
          n.env e.env g.env)
  (force-output)
  (let ((subgraph (lookup 'subgraph g.env)))
    (remove nil (mapcar (lambda (statement)
                          (let ((evaluated-statement (remove end-of-statement
                                                             statement)))
                            (when evaluated-statement
                              (evaluate evaluated-statement
                                        (node.env subgraph)
                                        (edge.env subgraph)
                                        (graph.env subgraph)))))
                        (split-sequence:split-sequence end-of-statement e)))))

(defun lookup-or-create-node (exp nodes node.env)
  (handler-case
      (lookup exp nodes)
    (lookup-failure (c)
      (declare (ignore c))
      (make-instance 'node
                     :id exp
                     :node.env node.env))))

(defconstant +elements-for-graph+
  '((? strict)
    (or graph digraph)
    (? id)
    content))
(defconstant +max-elements-for-graph+
  (length +elements-for-graph+))

(defun evaluate-graph (exp)
  (let ((strictp (when (eq (car exp) 'strict)
                   (pop exp)
                   t))
        (g.env (extend () 'nodes nil))
        (graph-type (pop exp)))
    (declare (type (member graph digraph) graph-type))
    (let ((graph (make-instance
                  graph-type
                  :strict strictp
                  :id (when (atom (car exp)) (pop exp))
                  ;; TODO: global env
                  ;; :node.env  nil
                  ;; :edge.env  nil
                  )))
      ;; Set the 'graph binding in graph environment to the
      ;; graph... for later introspection needs.
      ;; Also set the contents of the graph to everything in the
      ;; thing. 
      (setf
       (graph.env graph) (print (extend g.env
                                        ;; Both subgraph and graph are
                                        ;; things.  Graph should never be
                                        ;; rebound, but subgraph will always
                                        ;; hold the current subgraph.
                                        '(subgraph graph)
                                        (list graph graph)))
       (contents graph)
       (if (consp (car exp))
           (evlis (car exp)
                  nil
                  nil
                  (graph.env graph))
           (error "Invalid graph")))
      graph)))

(defun read-graph (stream)
  "read and verify that the stream contains a graph type"
  ;; Get strictness and graphiness
  (evaluate-graph (loop
                    for i from 1 to 4
                    for exp = (read stream)
                    collecting exp
                    while (atom exp))))

(defun read-dot (stream)
  ;let ((*readtable* *graph-readtable*))
  (with-square-list-reading
    (with-curly-reading
      (with-eos-reading #\;
        (read-graph stream)))))

(defun read-dot-from-string (string)
  (read-dot (make-string-input-stream string)))

