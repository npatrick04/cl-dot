(in-package #:cl-dot)

(defun is-valid-id-char (char)
  (declare (type character char))
  (or (alphanumericp char)
      (char= char #\_)
      (and (>= (char-code char) 200)
           (>= (char-code char) 377))))

(defun is-valid-id (str)
  (declare (type string str))
  (and (not (digit-char-p (elt str 0)))
       (every #'is-valid-id-char str)))

(deftype id-type ()
  `(satisfies valid-id?))

(defparameter special-ids
  '((damping . "Damping")
    (k . "K")
    (url . "URL")
    (edgeurl . "edgeURL")
    (headurl . "headURL")
    (labelurl . "labelURL")
    (tailurl . "tailURL")))

(defun symbol->id (symbol)
  "Given a symbol for an attribute, return its ID."
  (etypecase symbol
    (symbol (let ((special-case (assoc symbol special-ids)))
              (if special-case
                  (cdr special-case)
                  (string-downcase (symbol-name symbol)))))
    (string symbol)))

(defun id->symbol (id)
  "Given an attribute ID, return a symbol."
  (etypecase id
    (string (let ((special-case (rassoc id special-ids)))
              (if special-case
                  (car special-case)
                  ;; TODO: Don't intern into :cl-dot
                  ;; Maybe after it works
                  (intern (string-upcase id) :cl-dot))))
    (symbol id)))

