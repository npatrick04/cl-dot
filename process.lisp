(in-package #:cl-dot/process)

(defparameter *dot-program* "dot")

(defun generate-dot-input (a-graph)
  (lambda (out)
    (let ((cl-dot::*dot-print-type* :dot))
      (write a-graph :stream out))))

(defmacro def-readable-output (type)
  (let ((option (format nil "-T~(~A~)" (symbol-name type))))
    `(defun ,type (graph)
       ,(format nil "Pass graph through *dot-program* to generate ~(~A~) graph types." type)
       (uiop/run-program:run-program (list *dot-program* ,option)
                                     :output #'cl-dot:read-dot
                                     :input (generate-dot-input graph)))))

(defmacro def-output (type &optional (fname type))
  (let ((option (format nil "-T~(~A~)" (symbol-name type))))
    `(defun ,fname (graph &optional (file ,(format nil "~(out.~A~)" (symbol-name type))))
       ,(format nil "Generate ~(~A~) format files with *dot-program*" type)
       (uiop/run-program:run-program (list *dot-program* ,option "-o" file)
                                     :output nil
                                     :input (generate-dot-input graph)))))

;;; Readable types
(def-readable-output dot)
(def-readable-output xdot)

;;; File output types
(def-output dot dot-file)
(def-output xdot xdot-file)
(def-output ps ps-file)
(def-output svg svg-file)
(def-output svgz svgz-file)
(def-output fig fig-file)
(def-output png png-file)
(def-output gif gif-file)
(def-output imap imap-file)
(def-output cmapx cmapx-file)

;;; User-specified output in the case that others are supported via plugin or something else.
(defun render-output (graph type &optional (file (format nil "~(out.~A~)" type)))
  "Generate files of type by passing as a -Ttype argument to the *dot-program*.
type may be a symbol or string that is passed as -T with lowercase."
  (uiop/run-program:run-program (list *dot-program*
                                      (format nil "-T~(~A~)" type)
                                      "-o" file)
                                :output nil
                                :input (generate-dot-input graph)))
