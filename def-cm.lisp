(in-package :compute)

(defmacro defun-cm (&whole whole name lambda-list &body body)
  (multiple-value-bind (forms declarations docstring)
      (alexandria:parse-body body :documentation t :whole whole)
    `(defun ,name ,lambda-list
       ,@declarations
       ,@(if docstring
             (list docstring)
             nil)
       (cm ,@forms))))

(defun parse-defmethod-arguments (form)
  (pop form) ; defmethod
  (cm name <- (pop form)
      (values qualifiers rest) <- (take-while (complement #'listp)
                                              form)
      lambda-list <- (pop rest)

      (values body
              declarations
              docstring)
      <- (alexandria:parse-body rest
                                :documentation t
                                :whole form)

      (list :name name
            :qualifiers qualifiers
            :lambda-list lambda-list
            :declarations declarations
            :docstring docstring
            :body body)))

;; Test:
#+nil (parse-defmethod-arguments
       '(defmethod foo :asd (a b) (declare (number a b)) "asd" (+ a b)))

(defmacro defmethod-cm (&whole whole name &rest args)
  (declare (ignore name args))
  (cm parsed-form <- (parse-defmethod-arguments whole)

      (destructuring-bind (&key name qualifiers lambda-list
                                declarations
                                docstring
                                body)
          parsed-form

          `(defmethod ,name ,@qualifiers ,lambda-list
             ,@declarations
             ,@(if docstring
                   (list docstring)
                   nil)
             (cm ,@body)))))
