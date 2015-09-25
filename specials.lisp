(in-package :cl-dot)

#+:sbcl
(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defvar *indent* nil
  "Whether to insert line breaks and indent. Also controls amount of
indentation dynamically.")

(defvar *dot-mode* :xml
  ":dot is it presently")

(defconstant +newline+ (make-string 1 :initial-element #\Newline)
  "Used for indentation.")

(defconstant +spaces+ (make-string 2000
                                   :initial-element #\Space
                                   :element-type 'base-char)
  "Used for indentation.")

