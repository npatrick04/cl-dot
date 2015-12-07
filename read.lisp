;;; Copyright 2015 Nick Patrick

(in-package #:cl-dot)

(declaim (optimize debug))

;;; Utility rules

(defmacro with-reader-macros (assignments &body body)
  (let ((originals (gensym)))
    `(let ((,originals (list ,@(mapcar (lambda (assignment)
                                        `(get-macro-character ,(car assignment)))
                                      assignments))))
       (unwind-protect
            (progn
              ,@(mapcar (lambda (assignment)
                          `(set-macro-character ,@assignment))
                        assignments)
              ,@body)
         (mapcar (lambda (char original)
                   (set-macro-character char original))
                 ',(mapcar #'car assignments)
                 ,originals)))))

;;; Comment
;;This breaks character entry!!! Figure it out later.
(defmacro with-dot-comment-reading (&body body)
  `(with-reader-macros ((#\# (get-macro-character #\;)))
     ,@body))

;;; End of statement
(defparameter end-of-statement :eos)
(defparameter the-equal-flag :equal)

(defun end-of-statement (stream char)
  "TODO: Something here instead probably"
  (declare (ignore char stream))
  end-of-statement)
(defun the-equal-flag (stream char)
  "TODO: Something here instead probably"
  (declare (ignore char stream))
  the-equal-flag)
(defun nothing (stream char)
  "TODO: Something here instead probably"
  (declare (ignore char stream))
  (values))

(defmacro with-nothing-reading (chars &body body)
  `(with-reader-macros (,@(mapcar (lambda (char)
                                    `(,char #'nothing))
                                  chars))
     ,@body))
;; (defmacro with-eos-reading (char &body body)
;;   `(with-reader-macros ((,char #'end-of-statement))
;;      ,@body))
;; (defmacro with-equal-reading (&body body)
;;   `(with-reader-macros ((#\= #'the-equal-flag))
;;      ,@body))

(defvar *graph* nil
  "The current subgraph being constructed.")

(defun read-char+ (stream)
  (prog1 (read-char stream)
    (remove-whitespace stream)))

(defun read-attribute-list (stream)
  "[id1 = something; id2=else] => '((id1 . something) (id2 . else))"
  (when (char/= (read-char+ stream) #\[)
    (error "read-attribute-list isn't reading an attribute list..."))
  (do ((result ()))
      ((char= (peek-char nil stream) #\])
       (read-char+ stream)
       result)
    (let ((attr (read+ stream))
          (eq?  (read+ stream)))
      (if (eq eq? the-equal-flag)
          (push (cons attr (read+ stream))
                result)
          (error "Unexpected result reading attribute list ~A" eq?)))))

(define-constant +whitespace+
    '(#\Space #\Return #\Newline #\Vt #\Page #\Tab)
  :test #'equalp)

(defun remove-whitespace (stream)
  "Remove whitespace from the stream, returning t when at least one
newline has been encountered."
  (do* ((char (peek-char nil stream) (peek-char nil stream))
        (newline? (char= char '#\Newline) (or newline?
                                              (char= char '#\Newline))))
       ((not (some (lambda (ws) (char= char ws))
                   +whitespace+))
        newline?)
    (read-char stream t nil t)))

(defun read-dot-string (stream open-char)
  (let ((buf (make-array 10
                         :element-type 'character
                         :adjustable t
                         :fill-pointer 0)))
    (loop (let ((char (read-char stream)))
            (if (eq char open-char)
                (return (coerce buf 'string))
                (vector-push-extend char buf 10))))))

(defun read-attribute-lists (stream)
  "read one or more attribution lists. This is started by a macro
character on #\[, so at least one a-list is present. "
  ;; Read the first alist, then subsequent alists
  (with-reader-macros ((#\" #'read-dot-string))
    (do ((alist (read-attribute-list stream)
                (append alist (read-attribute-list stream)))
         ;; As long as the next character after the last alist
         (next-char (peek-char nil stream t nil t) (peek-char nil stream t nil t)))
        ;; is still regarding an alist.
        ((char/= next-char #\[)
                                        ;(format t "In read-attribute-lists, got ~A~%" alist)
         ;; Then return the set of concatenated alists.
         alist)
      ;; (format t "In read-attribute-lists, got ~A, reading again when next-char == ~A~%"
      ;;         alist next-char)
      )))

;; (defmacro with-square-list-reading (&body body)
;;   `(with-reader-macros ((#\[ #'read-attribute-lists)
;;                         (#\] (get-macro-character #\))))
;;      ,@body))

(defmacro with-dot-readtable (&body body)
  ;; TODO: Make this a static readtable
  `(with-reader-macros ((#\{ #'curly-reader)
                        (#\} (get-macro-character #\)))
                        (#\; #'nothing)
                        (#\, #'nothing)
                        (#\[ #'nothing)
                        (#\] #'nothing)
                        (#\= #'the-equal-flag)
                        (#\- #'read-edge)
                        (#\# (get-macro-character #\;)))
     ,@body))

(defun read-edge (stream char)
  (declare (ignore char))
  (let ((next (read-char+ stream)))
    (ecase next
      (#\> '->)
      (#\- '--))))

;;; Debug...
(defmacro lispmode (&body body)
  `(let ((*readtable* (copy-readtable nil)))
     ,@body))

(defparameter *dot-readtable*
  (named-readtables:defreadtable dot-readtable
    (:merge :standard)
    (:case :preserve))
  "A plain jane readtable, same as the standard one...except with case preserved.")

(defun lispify-symbol (symb)
  (if (is-valid-id (symbol-name symb))
      (id->symbol (symbol-name symb))
      (error "Invalid id ~A" (symbol-name symb))))

(defparameter *env.graph* () "The global graph environment.") 
(defparameter *env.node* () "The global node environment.") 
(defparameter *env.edge* () "The global edge environment.") 


;;; read subgraphs as a list of contents
(defun curly-reader (stream char)
  "This function is only invoked when reading anonymous subgraphs."
  (declare (ignore char) (optimize debug))
  (assert *graph*)
  (let* ((*graph* (make-instance
                    'subgraph
                    :node.env  (node.env *graph*)
                    :edge.env  (edge.env *graph*)
                    :graph.env (graph.env *graph*))))
    (declare (special *graph*))
    ;; Set the current subgraph to that of the one being created.
    (setf (graph.env *graph*) (extend (graph.env *graph*) 'subgraph *graph*)
          (contents *graph*)  (read-statement-list *graph* stream))
    *graph*))

(define-condition stmt-read-failed (condition)
  ((next :reader next :initarg :next :initform nil)))

(defun read+ (stream)
  (prog1
      (read stream)
    (remove-whitespace stream)))

(defgeneric get-nodes (element))
(defmethod get-nodes ((sg subgraph))
  (subgraph-nodes sg))
(defmethod get-nodes ((node node))
  (list node))

(defun read-edge-stmt (subgraph stream LHS)
  "Given the current subgraph, a stream and the left-hand-side
of an edge statement, read the rest."
  ;; Can be node-id or subgraph
  (let ((beginning-of-edge (peek-char nil stream)))
    (case beginning-of-edge
      (#\-
       (let ((edge-op (read+ stream))
             (RHS     (read+ stream)))
         (when (symbolp RHS)
           (setf RHS (lookup-or-create-node RHS (graph.env subgraph))))
         (if (eq edge-op (connector-style subgraph))
             (let ((edges (make-edges subgraph
                                      (get-nodes LHS)
                                      (get-nodes RHS))))
               (if (char/= (peek-char nil stream) #\;)
                   (multiple-value-bind (rest props) (read-edge-stmt subgraph stream RHS)
                     (when props
                       (mapc (lambda (edge)
                               (setf (specific.env edge) props))
                             edges))
                     (values (append (list edge-op RHS)
                                     rest)
                             props))
                   (values (list edge-op RHS) nil)))
             (error "Bad edge op ~A" edge-op))))
      (#\[
       ;; properties
       (values nil (read-attribute-lists stream)))
      (t (values nil nil)))))

(defun read-edge-statements (subgraph stream LHS)
  "Let read-edge-stmt handle constructing the edges...this function adds 
edge properties to the returned list."
  (multiple-value-bind (edges props) (read-edge-stmt subgraph stream LHS)
    (if props
        (append edges (list props))
        edges)))

(defun read-possibly-identified-subgraph (subgraph stream)
  ;; This could be named...let's check
  (let* ((maybe-id (when (char/= (peek-char nil stream) #\{)
                     (read+ stream)))
         (*graph* (make-instance
                   'subgraph
                   :id (when (symbolp maybe-id) maybe-id)
                   :node.env  (node.env subgraph)
                   :edge.env  (edge.env subgraph)
                   :graph.env (graph.env subgraph))))
    (declare (special *graph*))
    ;; Get rid of the #\{
    (read-char+ stream)
    (setf (graph.env *graph*) (extend (graph.env *graph*) 'subgraph *graph*)
          (contents *graph*)  (read-statement-list *graph* stream))
    *graph*))

(defun read-statement (subgraph stream)
  "This function does most of the work for reading dot statements."
  (remove-whitespace stream)
  ;; Check if we're prematurely done.
  (when (member (peek-char nil stream) '(#\} #\;) :test #'char=)
    (return-from read-statement (read-char stream)))
  ;; Get things started
  (let ((first-things (read+ stream)))
    (if (symbolp first-things)
        (case first-things
          ((|graph| |node| |edge|)
           (if (char= (peek-char nil stream) #\[)
               (let ((result (read-attribute-lists stream)))
                 (ecase first-things
                   (|graph| (setf (graph.env subgraph)
                                  (extend (graph.env subgraph)
                                          (mapcar #'car result)
                                          (mapcar #'cdr result))))
                   (|node| (setf (node.env subgraph)
                                 (extend (node.env subgraph)
                                         (mapcar #'car result)
                                         (mapcar #'cdr result))))
                   (|edge| (setf (edge.env subgraph)
                                 (extend (edge.env subgraph)
                                         (mapcar #'car result)
                                         (mapcar #'cdr result)))))
                 (list first-things result))
               (error "Expecting #\[ at position ~d"
                      (file-position stream))))
          ((|subgraph|)
           (read-possibly-identified-subgraph subgraph stream))
          (t ;; It's an ID for sure...but what kind of ID?
           (if (char= (peek-char nil stream) #\=)
               ;; Return an ID '=' ID statement
               (let ((eq (read-char+ stream))     ;eliminate the #\=
                     (value (read+ stream)))
                 (declare (ignore eq))
                 (setf (graph.env subgraph)
                       (extend (graph.env subgraph)
                               first-things
                               value))
                 (cons first-things value))
               ;; It's a node ID, so could be a node statement or edge statement.
               ;; Check if it's an edge...
               (let ((node (lookup-or-create-node first-things (graph.env subgraph))))
                 (if (char= (peek-char nil stream) #\-)
                     ;; It's got to be an edge!
                     (cons node (read-edge-statements subgraph stream node))
                     ;; It's got to be a node statement!
                     (progn
                       (when (char= (peek-char nil stream) #\[)
                         (setf (specific.env node) (read-attribute-lists stream)))
                       node))))))
        ;; It must be a subgraph!, read the edge if present.
        (if (char= (peek-char nil stream) #\-)
            ;; It's got to be an edge initiated by a subgraph!
            (cons first-things (read-edge-statements subgraph stream first-things))
            first-things))))

(defun read-statement-list (subgraph stream)
  "Given all but the initial #\{, read a subgraph definition
and return a list of the contents."
  ;; TODO: handler-bind end-of-file to signal the condition
  (accum stmt-list
    (do ((statement (read-statement subgraph stream)
                    (read-statement subgraph stream)))
        ((and (characterp statement) (char= statement #\})))
      (unless (and (characterp statement) (char= statement #\;))
        (stmt-list statement)))))

(defun read-graph (stream)
  "Read and verify that the stream contains a graph type"
  ;; Get strictness and graphiness
  (let ((exp (read+ stream)))
    (let* ((strictp (eq exp '|strict|))
           (graph-type (if strictp
                           ;; Need to read the graph type
                           (lispify-symbol (read+ stream))
                           (lispify-symbol exp))))
      (let* ((graph-id (when (char/= (peek-char nil stream) #\{)
                         (read+ stream)))
             (*graph* (make-instance
                       graph-type
                       :strict strictp
                       :id graph-id
                       :node.env *env.node*
                       :edge.env *env.edge*)))
        ;; Make sure we get the dynamic *graph* variable for use in tying back to
        ;; the real thing in reader macros.  Is there a non-dynamic variable method
        ;; for passing lexical data to reader macros???
        (declare (special *graph*))
        (setf (graph.env *graph*)
              (extend *env.graph*
                      ;; Both subgraph and graph are
                      ;; things.  Graph should never be
                      ;; rebound, but subgraph will always
                      ;; hold the current subgraph.
                      '(subgraph graph nodes)
                      (list *graph* *graph* nil)))
        ;; Set the contents of the graph to everything in the thing.
        (let ((next-char (read-char+ stream)))
          (if (char= next-char #\{)
              (setf (contents *graph*)
                    (read-statement-list *graph* stream))
              (error "Unexpected character ~A when reading graph." next-char)))
        *graph*))))

(defun read-dot (stream)
  ;; Is this legal or ok?
  (in-package :cl-dot)
  (let ((original-readtable *readtable*))
    (unwind-protect
         (progn
           (setf *readtable* *dot-readtable*)
           (with-dot-readtable
               (read-graph stream)))
      (setf *readtable* original-readtable))))

(defun read-dot-from-string (string)
  (read-dot (make-string-input-stream string)))

(defun read-dot-from-file (file)
  (with-open-file (in file)
    (read-dot in)))

#|

(cl-dot:read-dot-from-string "
digraph {
 foo[shape=oval];
}")

(cl-dot:read-dot-from-string "
digraph {
 subgraph {
  node[border=dashed];
  foo[shape=box][style=filled];
  bar[shape=oval];
 };
 subgraph {
  edge[style=dashed];
  foo->bar;
 }
 subgraph {
  edge[style=solid];
  bar->foo;
 }
}")

|#
