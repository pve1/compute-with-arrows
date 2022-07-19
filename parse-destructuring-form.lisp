(in-package :compute)

(defun parse-destructuring-form (destructuring-form expression rest)
  (multiple-value-bind (required optional rest-name keywords
                        allow-other-keys aux has-key)
      (alexandria:parse-ordinary-lambda-list destructuring-form)
    (let* (required*
           optional*
           keywords*
           rest-name*
           variable-gensym-pairs
           gensymmed-lambda-list)

      ;; (foo) -> #:G123

      (setf required* (loop :for name :in required
                            :for gensym = (gensym "REQUIRED")
                            :collect gensym
                            :do (push (list name gensym) variable-gensym-pairs)))

      ;; (&optional foo) -> #:G123
      ;; (&optional (foo 1)) -> (#:G123 1)

      (setf optional* (loop :for (name init suppliedp) :in optional
                            :for gensym = (gensym "OPTIONAL")
                            :collect (list gensym init)
                            :do (push (list name gensym) variable-gensym-pairs)))

      ;; (&key foo) -> ((:foo #:G123))
      ;; (&key (foo 1)) -> ((:foo #:G123) 1)

      (setf keywords* (loop :with keyword-name
                            :with name
                            :with init
                            :for k :in keywords
                            :for gensym = (gensym "KEYWORD")
                            :do (destructuring-bind ((keyword-name* name*) init* suppliedp*) k
                                  (setf keyword-name keyword-name*
                                        name name*
                                        init init*))
                            :collect (list (list keyword-name gensym) init)
                            :do (push (list name gensym) variable-gensym-pairs)))

      ;; (&rest rest) -> #:G123
      (when rest-name
        (setf rest-name* (gensym "REST"))
        (push (list rest-name rest-name*) variable-gensym-pairs))

      (setf gensymmed-lambda-list
            (append (when required
                      required*)
                    (when optional
                      (cons '&optional optional*))
                    (when rest-name
                      (cons '&rest (list rest-name*)))
                    (when keywords
                      (cons '&key keywords*))
                    (when allow-other-keys
                      (list '&allow-other-keys))))

      (setf variable-gensym-pairs (nreverse variable-gensym-pairs))

      (list :form `(destructuring-bind ,gensymmed-lambda-list ,expression
                     (setf ,@(alexandria:flatten variable-gensym-pairs)))
            :variables (mapcar #'first variable-gensym-pairs)
            :rest rest))))
