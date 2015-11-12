(in-package :cl-dot)
(declaim (optimize debug 3))

(defun evaluate (e n.env e.env g.env)
  (if (atom e)
      (cond ((symbolp e) e)
            ((or (numberp e) (stringp e) (characterp e) (vectorp e))
             e)
            
            (t (error "Cannot evaluate: ~A" e)))
      (case (car e)
        (subgraph
         (let ((name (when (symbolp (cadr e))
                       (cadr e)))
               (this.g.env (extend g.env 'nodes ())))
           (make-instance (car e)
                          :id name
                          :graph.env this.g.env
                          :node.env  n.env
                          :edge.env  e.env
                          :contents (evlis
                                     (if (symbolp (cadr e))
                                         (caddr e)
                                         (cadr e))
                                     n.env
                                     e.env
                                     this.g.env))))
        ;; (node ((shape . bar))
        ;;(node (cons (cadr e n.env)))
        ;;(edge (cons (cadr e e.env)))
        (t
         (if (extend g.env (car e) (caddr e))
             (let* ((nodes (lookup 'nodes g.env))
                    (node (lookup-or-create-node e nodes n.env)))
               (format t "nodes: ~A~%node: ~A~%" nodes node)
               (when (consp (cadr e))
                 (setf (specific.env node) (cadr e)))
               (update 'nodes g.env (extend nodes (car e) node))
               node))))))

(defun evlis (e n.env e.env g.env)
  (mapcar (lambda (statement)
            (evaluate statement n.env e.env g.env))
          (split-sequence:split-sequence end-of-statement e)))

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
  (let ((g.env (extend ()
                       (list 'nodes
                             'strict)
                       (list nil
                             (when (eq (car exp) 'strict)
                               (pop exp)
                               t))))
        (graph-type (pop exp)))
    (declare (type (member graph digraph) graph-type))
    (make-instance
     graph-type
     :id (when (atom (car exp)) (pop exp))
     ;; TODO: global env
     :graph.env g.env 
     ;; :node.env  nil
     ;; :edge.env  nil
     :contents (if (consp (car exp))
                   (evlis (car exp)
                          nil
                          nil
                          g.env)
                   (error "Invalid graph")))))

(defun read-graph (stream)
  "read and verify that the stream contains a graph type"
  ;; Get strictness and graphiness
  (evaluate-graph (loop
                    for i from 1 to 4
                    for exp = (read stream)
                    collecting exp
                    while (atom exp))))

(defun read-dot (stream)
  (let ((*readtable* *graph-readtable*))
    (with-square-list-reading
      (with-curly-reading
        (with-eos-reading #\;
          (read-graph stream))))))

(defun read-dot-from-string (string)
  (read-dot (make-string-input-stream string)))

