(in-package #:cl-dot)

(deftype graph-print-method ()
  '(member unreadable dot))

(declaim (type graph-print-method *dot-print-type*))
(defvar *dot-print-type* 'dot
  "This variable determines how a graph object is printed.")

(defun print-alist (alist stream &optional (bracket t))
  (when alist
    (format stream "~:[~;[~]~{~A~^, ~}~:[~;]~]"
            bracket
            (loop for (attr . value) in alist
               collect (format nil "~{~A=\"~A\"~}"
                               (mapcar #'symbol->id (list attr value))))
            bracket)))

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

(defmethod print-object ((object subgraph) stream)
  (case *dot-print-type*
    (unreadable
     (print-unreadable-object (object stream :type t :identity t)))
    (dot
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
             (statements object)))))

(defmethod print-object :after ((object environment) stream)
  (case *dot-print-type*
    (unreadable
     (print-unreadable-object (object stream :type t :identity t)))
    (dot
     (print-alist (attributes object) stream))))
(defmethod print-object ((object node) stream)
  (case *dot-print-type*
    (unreadable
     (print-unreadable-object (object stream :type t :identity t)))
    (dot
     (format stream "~A" (string-downcase (id object))))))
(defmethod print-object ((object edge) stream)
  (case *dot-print-type*
    (unreadable
     (print-unreadable-object (object stream :type t :identity t)))
    (dot
     (format stream "~A -> ~A"
             (string-downcase (id (car (nodes object))))
             (string-downcase (id (cadr (nodes object))))))))
                                        ;TODO: add attributes
