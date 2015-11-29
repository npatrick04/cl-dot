(in-package :cl-dot)
(declaim (optimize debug))

(defun evaluate-symbol (symb)
  ;; Here's where we lispify
  (if (is-valid-id (symbol-name symb))
      (progn
        ;; (format t "lispifying ~A into ~A~%"
        ;;         (symbol-name symb)
        ;;         (id->symbol (symbol-name symb)))
        (id->symbol (symbol-name symb)))
      (error "Invalid id ~A" (symbol-name symb))))

(defun evaluate (e n.env e.env g.env)
  (if (atom e)
      (cond ((symbolp e) (evaluate-symbol e))
            ((or (numberp e) (stringp e) (characterp e) (vectorp e))
             e)
            (t (error "Cannot evaluate: ~A" e)))
      (if (symbolp (car e))
          (let ((symb (evaluate-symbol (car e))))
            (with++ index 1
              (case symb
                (subgraph
                 (let ((name (when (symbolp (nth index e))
                               (evaluate-symbol (nth index++ e)))))
                   (let ((subgraph (make-instance 'subgraph
                                                  :id name
                                                  :node.env  n.env
                                                  :edge.env  e.env)))
                     (let ((new-graph-env (extend g.env 'subgraph subgraph)))
                       (setf
                        (graph.env subgraph) new-graph-env
                        (contents subgraph) (evlis
                                             (nth index++ e)
                                             n.env
                                             e.env
                                             new-graph-env))
                       (when (nth index e)
                         (warn "Expression not completely evaluated: ~A"  e))
                       subgraph))))
                ;; (node ((shape . bar))
                (node
                 (if (consp (cadr e))
                     ;; Looks like a node environment extension...I guess.
                     (progn
                       (setf (node.env (lookup 'subgraph g.env))
                             (append (cadr e) n.env))
                       (list symb (cadr e)))
                     
                     (error "The node statement requires a list of parameters: ~A"
                            (cadr e))))

                (edge
                 (if (consp (cadr e))
                     ;; Looks like a edge environment extension...I guess.
                     (progn
                       (setf (edge.env (lookup 'subgraph g.env))
                             (append (cadr e) e.env))
                       (list symb (cadr e)))
                     
                     (error "The edge statement requires a list of parameters: ~A"
                            (cadr e))))
                ;; Nodes, edges
                (t
                 (let ((node (lookup-or-create-node symb g.env)))
                   (if (consp (cadr e))
                       ;; finish the node definition by reading any alists.
                       (progn
                         (setf (specific.env node)
                               (append (cadr e) (specific.env node)))
                         node)

                       ;; Define an edge
                       (let ((graph (lookup 'graph g.env))
                             (attributes? (let ((last (car (last e))))
                                            (when (consp last) last))))
                         (cond
                           ((eq (cadr e) (connector-style graph))
                            (flet ((make-edges (source dest)
                                     (let ((source (lookup-or-create-node (evaluate-symbol source) g.env))
                                           (dest   (lookup-or-create-node (evaluate-symbol dest) g.env)))
                                       (typecase graph
                                         (digraph (make-digraph-edge source dest attributes? e.env))
                                         (graph   (make-graph-edge source dest attributes? e.env))))))
                              (do* ((expression (if attributes? (butlast e) e)
                                                (cddr expression))
                                    (result     (make-edges (car expression) (caddr expression))
                                                (append (make-edges (car expression) (caddr expression)) result)))
                                   ((<= (length expression) 3)
                                    e))))
                           ((null (cadr e)))    ;do nothing 
                           (t (error "Unrecognized expression ~A" e)))))
                   ;; (update 'nodes g.env (extend nodes symb node))
                   )))))
          ;; Not a symbolp...anonymous subgraph is the thing.
          ;; TODO, anonymous subgraph
          ;; TODO, graph stuff
          (cond
            ((eq (cadr e) the-equal-flag)
             (setf (graph.env (lookup 'subgraph g.env))
                   (append (cadr e) e.env))))
          )))

(defun evlis (e n.env e.env g.env)
  (declare (ignorable n.env e.env))
  ;; (format t "~&evlis ~A~%  n.env: ~A~%  e.env: ~A~%  g.env: ~A~%" e
  ;;         n.env e.env g.env)
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

(define-constant +elements-for-graph+
  '((? strict)
    (or graph digraph)
    (? id)
    content))
(define-constant +max-elements-for-graph+
  (length +elements-for-graph+))

(defun evaluate-graph (exp)
  ;; eliminate fancy business
  (assert (<= (length exp) 4))
  (with++ index 0
    (let ((strictp (when (eq (evaluate (nth index exp) () () ())
                             'strict)
                     (incf index)
                     t))
          (g.env (extend () 'nodes nil))
          (graph-type (evaluate (nth index++ exp) () () ())))
      (declare (type (member graph digraph) graph-type))
      (let ((graph (make-instance
                    graph-type
                    :strict strictp
                    :id (when (atom (nth index exp))
                          (evaluate (nth index++ exp) () () ()))
                    ;; TODO: global env
                    )))
        ;; Set the 'graph binding in graph environment to the
        ;; graph... for later introspection needs.
        ;; Also set the contents of the graph to everything in the
        ;; thing. 
        (setf
         (graph.env graph) (extend g.env
                                   ;; Both subgraph and graph are
                                   ;; things.  Graph should never be
                                   ;; rebound, but subgraph will always
                                   ;; hold the current subgraph.
                                   '(subgraph graph)
                                   (list graph graph))
         (contents graph)
         (if (consp (nth index exp))
             (evlis (nth index exp)
                    nil
                    nil
                    (graph.env graph))
             (error "Invalid graph")))
        graph))))


