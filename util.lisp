(in-package :compute-with-arrows)

(defun unexport-all (&optional (package *package*))
  (when (find-package package)
    (do-external-symbols (s package)
      (unexport s package))))

(defmacro clear-exports (&optional (package '*package*))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unexport-all ,package)))

(defun hop (key test &optional object)
  (setf key (alexandria:ensure-function key)
        test (alexandria:ensure-function test))
  (if object
      (lambda (other-object)
        (funcall test object (funcall key other-object)))
      (lambda (other-object)
        (funcall test (funcall key other-object)))))

(defun take-while (predicate list)
  (let (remaining)
    (values (loop :for head :on list
                  :for element = (car head)
                  :while head
                  :if (funcall predicate element)
                    :collect element
                  :else
                    :do (setf remaining head)
                        (loop-finish))
            remaining)))

(defun lambda-list-keyword-p (symbol)
  (member symbol lambda-list-keywords))

(defun substitute-using-map (tree map &key (test #'eql))
  (labels ((walk (tr)
             (typecase tr
               (cons (cons (walk (car tr))
                           (walk (cdr tr))))
               (null nil)
               (t (let ((subst? (assoc tr map :test test)))
                    (if subst?
                        (second subst?)
                        tr))))))
    (walk tree)))

(defun take-n (n list &key errorp &aux rest)
  (values (loop :for tail :on list
                :for k :from 1 :to n
                :collect (car tail)
                :finally (progn
                           ;; got less than desired amount?
                           (when (and errorp (not (<= n k)))
                             (error "End of list."))
                           (setf rest tail)))
          rest))

(defun symbol-with-name-p (symbol name)
  (and (symbolp symbol)
       (string= (symbol-name symbol) name)))
