(in-package :cl-dot)

(define-condition lookup-failure (error)
  ((binding :reader binding :initarg :binding))
  (:report
   (lambda (condition stream)
     (format stream
             "No such binding ~A" (binding condition)))))

(defun lookup (id env &key (test #'eq))
  (if (consp env)
      (if (funcall test (caar env) id)
          (cdar env)
          (lookup id (cdr env)))
      (error 'lookup-failure :binding id)))

(defun rev-lookup (key env &key (test #'eq))
  (if (consp env)
      (if (funcall test (cdar env) key)
          (caar env)
          (rev-lookup key (cdr env)))
      (error 'lookup-failure :binding key)))

(defun update (id env value &key (test #'eq))
  (if (consp env)
      (if (funcall test (caar env) id)
          (setf (cdar env) value)
          (update id (cdr env) value))
      (error 'lookup-failure :binding id)))

(defun extend (env variables values)
  (cond ((consp variables)
         (if (consp values)
             (cons (cons (car variables) (car values))
                   (extend env (cdr variables) (cdr values)))
             (error "Too less values")))
        ((null variables)
         (if (null values)
             env
             (error "Too much values")))
        ((symbolp variables) (cons (cons variables values) env))))
