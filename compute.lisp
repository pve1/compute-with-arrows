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

(defun substitute-assignment (tree)
  (destructuring-bind (&optional a op (b nil b-supplied-p) &rest rest) tree
    (cond ((and b-supplied-p (symbol-with-name-p op "<-"))
           (if (looks-like-destructuring-form-p a)
               (parse-destructuring-form* a b rest)
               (make-substitution :new `(noans* (setf ,a ,b))
                                  :consumed 3
                                  :rest rest)))
          ((and b-supplied-p (symbol-with-name-p op "->"))
           (if (looks-like-destructuring-form-p b)
               (parse-destructuring-form* b a rest)
               (make-substitution :new `(noans* (setf ,b ,a))
                                  :consumed 3
                                  :rest rest)))
          (t nil))))

(defun introduce-return-block (tree)
  `(block done
     ,tree))

(defun substitute-return (tree)
  (destructuring-bind (&optional op (a nil a-supplied-p) &rest rest) tree
    (when (and a-supplied-p
               (symbol-with-name-p op "^"))
      (make-substitution :new `(noans* (return-from done ,a))
                         :consumed 2
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
(defmacro cm1 (form &rest forms)
  (let ((i (gensym)))
    `(cm ,form ,i <- ,(intern "ANS" *package*) ,@forms ,i)))
