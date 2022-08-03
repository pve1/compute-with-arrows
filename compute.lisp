(in-package :compute-with-arrows)

(defun introduce-bindings (tree)
  (let* ((variables nil)
         (assign-fn (lambda (a op b)
                      (declare (ignore op b))
                      (cond ((symbolp a)
                             (pushnew a variables))
                            ((and (consp a)
                                  (eq 'values (car a)))
                             (dolist (var (rest a))
                               (pushnew var variables))))))
         (assign-left (lambda (a <- b)
                        (funcall assign-fn a <- b)))
         (assign-right (lambda (a -> b)
                        (funcall assign-fn b -> a)))
         (match-fn
           (lambda (tr)
             (match-binop tr (list (cons "<-" assign-left)
                                   (cons "->" assign-right))))))
    (walk-tree-conses match-fn tree)
    (setf variables (nreverse variables))
    (if variables
        (let ((vars (substitution-table-get 'variables)))
          (substitution-table-set 'variables (append variables vars))
          `(let ,variables
             ,tree))
        tree)))

(defun introduce-destructuring-bindings (tree)
  (let* ((variables nil)
         (assign-fn
           (lambda (a op b) ;; <-
             (declare (ignore op))
             (when (looks-like-destructuring-form-p a)
               (let* ((result (parse-destructuring-form a b nil))
                      (vars (getf result :variables)))
                 (dolist (v vars)
                   (pushnew v variables))))))
         (assign-left (lambda (a <- b)
                        (funcall assign-fn a <- b)))
         (assign-right (lambda (a -> b)
                         (funcall assign-fn b -> a)))
         (match-fn
           (lambda (tr)
             (match-binop tr (list (cons "<-" assign-left)
                                   (cons "->" assign-right))))))
    (walk-tree-conses match-fn tree)
    (setf variables (nreverse variables))
    (if variables
        (let ((vars (substitution-table-get 'variables)))
          (substitution-table-set 'variables (append variables vars))
          `(let ,variables
             ,tree))
        tree)))

(defun looks-like-destructuring-form-p (form)
  (and (consp form)
       (loop :for object :in form
             :thereis (member object '(&optional &key &rest &aux &allow-other-keys)))))

(defun parse-destructuring-form* (form expression rest)
  (let ((result (parse-destructuring-form form expression rest)))
    (make-substitution :new `(noans* ,(getf result :form))
                       :rest rest)))

(defmacro noans* (form)
  form)

(defun substitute-assignment (tree &key (allow-destructuring-p t))
  (destructuring-bind (&optional a op (b nil b-supplied-p) &rest rest) tree
    (cond ((and b-supplied-p (symbol-with-name-p op "<-"))
           (if (looks-like-destructuring-form-p a)
               (progn
                 (if allow-destructuring-p
                     (parse-destructuring-form* a b rest)
                     (error "Destructuring not allowed here.")))
               (if (symbol-with-name-p (car rest) "<-")
                   (let ((chain (substitute-assignment
                                 (cons b rest)
                                 :allow-destructuring-p nil)))
                     (make-substitution :new `(noans* (setf ,a ,(substitution-new chain)))
                                        :rest (substitution-rest chain)))
                   (make-substitution :new `(noans* (setf ,a ,b))
                                      :rest rest))))
          ((and b-supplied-p (symbol-with-name-p op "->"))
           (if (looks-like-destructuring-form-p b)
               (progn
                 (if allow-destructuring-p
                     (parse-destructuring-form* b a rest)
                     (error "Destructuring not allowed here.")))
               (make-substitution :new `(noans* (setf ,b ,a))
                                  :rest rest)))
          (t nil))))

(defun s-possessive-p (thing)
  (and (consp thing)
       (eq (car thing) 'quote)
       (symbol-with-name-p (cadr thing) "S")))

(defun substitute-access (tree)
  (destructuring-bind (&optional a op (b nil b-supplied-p)
                       &rest rest)
      tree
    (cond ((and b-supplied-p (s-possessive-p op))
           (substitute-access-s tree))
          ((and b-supplied-p (symbol-with-name-p op "OF"))
           (substitute-access-of tree)))))

(defun substitute-access-s (tree)
  (destructuring-bind (&optional object op (property nil property-supplied-p)
                         &rest rest)
        tree
      (when (and property-supplied-p (s-possessive-p op))
        (let ((chain? (s-possessive-p (car rest))))
          (if chain?
              (let ((this `(access ,object ',property)))
                (substitute-access (list* this rest)))
              (make-substitution
               :new `(access ,object ',property)
               :rest rest))))))

(defun substitute-access-of (tree &key (allow-s-possessive-p t))
  (destructuring-bind (&optional property op (object nil object-supplied-p)
                       &rest rest)
      tree
    (when (and object-supplied-p (symbol-with-name-p op "OF"))
      (let ((of-chain? (symbol-with-name-p (car rest) "OF"))
            (s-chain? (s-possessive-p (car rest))))
        (cond (s-chain?
               (unless allow-s-possessive-p
                 (error "Cannot have s-possessive here."))
               (let ((next (substitute-access (cons object rest))))
                 (make-substitution
                  :new `(access ,(substitution-new next) ',property)
                  :rest (substitution-rest next))))
              (of-chain?
               (let ((next (substitute-access (cons object rest))))
                 (make-substitution
                  :new `(access ,(substitution-new next) ',property)
                  :rest (substitution-rest next))))
              (t (make-substitution
                  :new `(access ,object ',property)
                  :rest rest)))))))

(defun introduce-return-block (tree)
  `(block done
     ,tree))

(defun substitute-return (tree)
  (destructuring-bind (&optional op (a nil a-supplied-p) &rest rest) tree
    (when (and a-supplied-p
               (symbol-with-name-p op "^"))
      (make-substitution :new `(noans* (return-from done ,a))
                         :rest rest))))

(defun mark-start-of-body (body-marker tree)
  (cons body-marker tree))

(defun car-eql-hop (object)
  (lambda (cons)
    (eql (car cons) object)))

(defun car-member-hop (set)
  (lambda (cons)
    (member (car cons) set)))

(defun mark-forms (list-or-predicate marker tree)
  (typecase list-or-predicate
    (list (if list-or-predicate
              (substitute-tree-conses
               (car-member-hop list-or-predicate)
               (lambda (tr)
                 (list marker tr))
               tree)
              tree))
    (t (substitute-tree-conses
        list-or-predicate
        (lambda (tr)
          (list marker tr))
        tree))))

(defun introduce-toplevel-chain (variable body-marker tree)
  (substitute-tree-conses
   (car-eql-hop body-marker)
   (lambda (tr)
     (let* ((need-variable-p nil)
            (forms
              (loop :for head :on (cdr tr)
                    :collect (if (and (consp (car head))
                                      (member (caar head) '(noans*)))
                                 (car head)
                                 (progn
                                   (setf need-variable-p t)
                                   `(setf ,variable ,(car head)))))))
       (let ((vars (substitution-table-get 'variables)))
         (when (member variable vars)
           (warn "Assignment to implicit toplevel chain variable ~S."
                     variable)))
       (if need-variable-p
           `(let (,variable)
              ,variable                 ;  To shut SBCL up.
              ,@forms)
           `(progn ,@forms))))
   tree))

(defmacro compute (&whole whole var-or-options
                   &body forms)
  (let* ((op (car whole))
         (body (load-time-value (make-symbol "BODY"))))
    (flet ((doit (variable &key skip-forms)
             (perform-substitution-passes
              `(,@(if variable
                      `((mark-start-of-body ,body))
                      `((mark-start-of-body progn)))
                (mark-forms (,op ,@skip-forms) skip*)
                (perform-substitutions (substitute-access))
                introduce-bindings
                introduce-destructuring-bindings
                introduce-return-block
                (perform-substitutions (substitute-return
                                        substitute-assignment))
                ,@(when variable
                    `((introduce-toplevel-chain ,variable ,body))))
              forms)))
      (typecase var-or-options
        (null (doit nil))
        (list (destructuring-bind (variable &key skip-forms) var-or-options
                (doit variable :skip-forms skip-forms)))
        (symbol (doit var-or-options))
        (t (error "Invalid argument ~S." var-or-options))))))

(defmacro cm (&rest forms)
  `(compute (,(intern "ANS" *package*) :skip-forms (cm cm1)) ,@forms))

;; Always returns the first form.
;; TODO: What if the first op is foo <- bar?
(defmacro cm1 (form &rest forms)
  (let ((i (gensym)))
    (cond ((symbol-with-name-p (car forms) "<-")
           (assert (symbolp form))
           (setf i form)
           `(cm ,form ,@forms ,i))

          ((symbol-with-name-p (car forms) "->")
           (assert (symbolp (cadr forms)))
           (setf i (cadr forms))
           `(cm ,form ,@forms ,i))

          (t `(cm ,i <- ,form
                  ,i
                  ,@forms
                  ,i)))))
