(in-package #:cl-dot)

(deftype graph-print-method ()
  '(member :unreadable :dot))

(declaim (type graph-print-method *dot-print-type*))
(defvar *dot-print-type* :dot
  "This variable determines how a graph object is printed.")

(defun print-alist (alist &optional (stream *standard-output*) (bracket t))
  (when alist
    (format stream "~:[~;[~]~{~A~^, ~}~:[~;]~]"
            bracket
            (loop for (attr . value) in alist
                  collect (format nil "~A=\"~A\"" attr value))
            bracket)))

(defparameter *dot-print-level* 0)
(defun incf-print-level (&optional (spaces 2))
  (incf *dot-print-level* spaces))
(defun decf-print-level (&optional (spaces 2))
  (decf *dot-print-level* spaces))

(declaim (inline print-spaces))
(defun print-spaces (&optional (level *dot-print-level*))
  (make-string level :initial-element #\Space))

(defun print-edge (statement stream)
  (loop for (LHS edge-op) on statement by #'cddr
        do (progn
             (typecase LHS
               (node (princ (symbol-name (id LHS)) stream))
               (subgraph (write LHS :stream stream)))
             (and edge-op (if (symbolp edge-op)
                              (princ edge-op stream)
                              (print-alist edge-op stream)))))
  (write-char #\; stream))

(defmethod print-object ((object subgraph) stream)
  (case *dot-print-type*
    (:unreadable
     (print-unreadable-object (object stream :type t :identity t)
       (when (id object)
         (write (id object) :stream stream))))
    (:dot
     (typecase object
       (graph
        (format stream "~@[strict ~]~(~A~) "
                (graph-strict object)
                (type-of object)))
       (t (write-string "subgraph " stream)))
     (when (id object)
       (princ (id object) stream))
     (format stream "~&~A{~%" (print-spaces))
     (incf-print-level)
     (dolist (statement (contents object))
       (fresh-line stream)
       (write-string (print-spaces) stream)
       (if (consp statement)
           (cond
             ((and (symbolp (car statement))
                   (member (symbol-name (car statement))
                           '("edge" "node" "graph")
                           :test #'string=))
              (format stream "~A~@[~A~];~%"
                      (car statement)
                      (print-alist (cadr statement) nil)))
             ((and (consp (cdr statement))
                   (eq (cadr statement)
                       (connector-style (lookup 'graph (graph.env object)))))
              (print-edge statement stream))
             (t
              ;; Not an attribute statement, subgraph, node or edge.
              ;; It must be a bare ID = ID; form.
              (format stream "~A=\"~A\";~%" (car statement) (cdr statement))))
           (format stream "~A;~&" statement)))
     (decf-print-level)
     (format stream "~&~A}" (print-spaces)))))

(defmethod print-object ((object node) stream)
  (case *dot-print-type*
    (:unreadable
     (print-unreadable-object (object stream :type t :identity t)
       (write (id object) :stream stream)))
    (:dot
     (format stream "~A~@[~A~]"
             (symbol-name (id object))
             (print-alist (specific.env object) nil)))))
