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

;;; read subgraphs as a list of contents
(defun curly-reader (stream char)
  (declare (ignore char) (optimize debug))
  (read-delimited-list #\} stream t))

;; (defmacro with-curly-reading (&body body)
;;   `(with-reader-macros ((#\{ #'curly-reader)
;;                         (#\} (get-macro-character #\)))
;;                         (#\; #'end-of-statement))
;;      ,@body))

(defun plist-to-alist (plist)
  "Not tail recursive"
  (declare (optimize debug))
  (if (cdr plist)
      (if (eq (cadr plist) the-equal-flag)
          (if (cdddr plist)
              (cons (cons (car plist) (caddr plist))
                    (plist-to-alist (cdddr plist)))
              (cons (cons (car plist) (caddr plist))
                    nil))
          (error "Malformed association list"))
      (if (car plist)
          (error "bad number of elements")
          ())))

(defun read-attribute-list (stream)
  "[id1 = something; id2=else] => '((id1 . something) (id2 . else))"
  ;; TODO: Concatenate attribute lists... it's an optional thing.
  (with-nothing-reading (#\; #\,)
    (plist-to-alist (read-delimited-list #\] stream t))))

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(define-constant +whitespace+
    '(#\Space #\Return #\Newline #\Vt #\Page #\Tab))

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

(defun read-attribute-lists (stream char)
  "read one or more attribution lists. This is started by a macro
character on #\[, so at least one a-list is present. "
  (declare (ignore char)
           (optimize debug))
  ;; Read the first alist, then subsequent alists
  (with-nothing-reading (#\[)
    (do ((alist (read-attribute-list stream)
                (append alist (read-attribute-list stream)))
         ;; Remove whitespace, while also detecting newline passage.
         (newline? (remove-whitespace stream) (remove-whitespace stream))
         ;; As long as the next character after the last alist
         (next-char (peek-char nil stream t nil t) (peek-char nil stream t nil t)))
        ;; is still regarding an alist.
        ((not (char= next-char #\[))
         ;; when we stop getting alists, return the concatenated
         ;; things, forcing and end-of-statement if we just went over

         ;; An experimental thing...
         (when (char/= next-char #\;)
           ;; If we know we aren't getting the end-of-statement we
           ;; need...
           ;(format t "Set end-of-statement for ~A~%" next-char)
           (let ((current-char-macro (get-macro-character next-char)))
             ;; Set the character we know we are getting...
             (set-macro-character next-char

                                  ;; To a function that returns what
                                  ;; we need, then force a re-read of
                                  ;; that character.
                                  (lambda (stream char)
                                    (set-macro-character char current-char-macro)
                                    (unread-char char stream)
                                    end-of-statement))))

         ;; Then return the set of concatenated alists.
         alist)
      ;; ;; Eliminate the macro-character
      ;; (read-char stream)
      ;; Reset the newline detection.
      (setf newline? nil))))

;; (defmacro with-square-list-reading (&body body)
;;   `(with-reader-macros ((#\[ #'read-attribute-lists)
;;                         (#\] (get-macro-character #\))))
;;      ,@body))

(defmacro with-dot-readtable (&body body)
  ;; TODO: Make this a static readtable
  `(with-reader-macros ((#\[ #'read-attribute-lists)
                        (#\] (get-macro-character #\)))
                        (#\{ #'curly-reader)
                        (#\} (get-macro-character #\)))
                        (#\; #'end-of-statement)
                        (#\= #'the-equal-flag)
                        (#\; #'end-of-statement)
                        (#\- #'read-edge)
                        (#\# (get-macro-character #\;)))
     ,@body))

(defun read-edge (stream char)
  (declare (ignore char))
  (let ((next (read-char stream)))
    (ecase next
      (#\> '->)
      (#\- '--))))

(defparameter *dot-readtable*
  (named-readtables:defreadtable dot-readtable
    (:merge :standard)
    (:case :preserve))
  "A plain jane readtable, same as the standard one...except with case preserved.")
