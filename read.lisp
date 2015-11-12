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

;;; read subgraphs as a list of contents
(defmacro with-curly-reading (&body body)
  `(with-reader-macros ((#\{ (lambda (stream char)
                               (declare (ignore char))
                               (read-delimited-list #\} stream)))
                        (#\} (get-macro-character #\))))
     ,@body))

;;; Comment
;;This breaks character entry!!! Figure it out later.
(defmacro with-hash-comment-reading (&body body)
  `(with-reader-macros ((#\# (get-macro-character #\;)))
     ,@body))

;;; End of statement
(defparameter end-of-statement :eos)

(defun end-of-statement (stream char)
  "TODO: Something here instead probably"
  (declare (ignore char stream))
  end-of-statement)
(defun nothing (stream char)
  "TODO: Something here instead probably"
  (declare (ignore char stream))
  (values))

(defmacro with-nothing-reading (char &body body)
  `(with-reader-macros ((,char #'nothing))
     ,@body))
(defmacro with-eos-reading (char &body body)
  `(with-reader-macros ((,char #'end-of-statement))
     ,@body))

(defun plist-to-alist (plist)
  "Not tail recursive"
  (declare (optimize debug))
  (if (cdr plist)
      (if (cddr plist)
          (cons (cons (car plist) (cadr plist))
                (plist-to-alist (cddr plist)))
          (cons (cons (car plist) (cadr plist))
                nil))
      (if (car plist)
          (error "bad number of elements")
          ())))

(defun read-attribute-list (stream char)
  "[id1 = something; id2=else] => '((id1 . something) (id2 . else))"
  (declare (ignore char))
  (with-nothing-reading #\=
    (with-nothing-reading #\;
      (plist-to-alist (read-delimited-list #\] stream)))))

(defmacro without-list-reading (&body body)
  ;; TODO
  ;; `(with-reader-macros ((#\[ #'read-attribute-list)
  ;;                       (#\] (get-macro-character #\))))
  ;;    ,@body)
  )

(defmacro with-square-list-reading (&body body)
  `(with-reader-macros ((#\[ #'read-attribute-list)
                        (#\] (get-macro-character #\))))
     ,@body))

(defparameter *graph-readtable*
  (copy-readtable nil)
  "A plain jane readtable, same as the standard one.")
