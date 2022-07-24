(in-package :compute-with-arrows)

(defvar *skip-cons-predicate* (constantly nil))

(defun walk-tree-conses (fn tree)
  (labels ((walk (tr)
             (typecase tr
               (cons
                (unless (funcall *skip-cons-predicate* tr)
                  (funcall fn tr)
                  (walk (car tr))
                  (walk (cdr tr))))
               (atom nil))))
    (walk tree)))

(defun substitute-tree-conses (predicate fn tree)
  (labels ((walk (tr)
             (typecase tr
               (cons
                (if (funcall *skip-cons-predicate* tr)
                    tr
                    (if (funcall predicate tr)
                        (funcall fn tr)
                        (cons (walk (car tr))
                              (walk (cdr tr))))))
               (atom tr))))
    (walk tree)))

(defun match-unop (tree unops &aux unop-entry)
  (destructuring-bind (&optional unop? a &rest rest) tree
    (declare (ignore rest))
    (when (and (symbolp unop?)
               (setf unop-entry (assoc unop? unops :test #'string=))
               (cdr unop-entry))
      (funcall (cdr unop-entry) unop? a))))

(defun match-binop (tree binops &aux binop-entry)
  (destructuring-bind (&optional a binop? b &rest rest) tree
    (declare (ignore rest))
    (when (and (symbolp binop?)
               (setf binop-entry (assoc binop? binops :test #'string=))
               (cdr binop-entry))
      (funcall (cdr binop-entry) a binop? b))))

(defun try-substitutions (substitutions tree)
  "Tries all substitution functions in SUBSTITUTIONS on TREE. Returns
the first result that succeeds, otherwise NIL."
  (loop :for s :in substitutions
        :for subst = (funcall s tree)
        :when (substitutionp subst)
          :return subst))

(defun perform-substitutions-1 (substitutions tree)
  (labels ((walk (tr)
             (typecase tr
               (cons (if (funcall *skip-cons-predicate* tr)
                         tr
                         (let* ((car (walk (car tr)))
                                (cdr (walk (cdr tr)))
                                (walked-subtree (cons car cdr))
                                (subst (try-substitutions substitutions
                                                          walked-subtree)))
                           (if (substitutionp subst)
                               (cons (substitution-new subst)
                                     (substitution-rest subst))
                               walked-subtree))))
               (atom tr))))
    (walk tree)))

(defun perform-substitutions (substitutions tree)
  (labels ((walk (tr)
             (typecase tr
               (cons (if (funcall *skip-cons-predicate* tr)
                         tr
                         (let* ((subst (try-substitutions substitutions tr)))
                           (if (substitutionp subst)
                               (cons (walk (substitution-new subst))
                                     (walk (substitution-rest subst)))
                               (cons (walk (car tr))
                                     (walk (cdr tr)))))))
               (atom tr))))
    (walk tree)))

(defvar *substitution-table* nil)

(defun substitution-table-get (symbol)
  (getf *substitution-table* symbol))

(defun substitution-table-set (symbol value)
  (setf (getf *substitution-table* symbol) value))

(defmacro skip* (form)
  form)

(defun perform-substitution-passes (substitution-passes tree)
  (let ((*substitution-table* nil)
        (*skip-cons-predicate* (car-member-hop '(skip* quote))))
    (flet ((doit (subst tr)
             (typecase subst
               ((or symbol function)
                (funcall subst tr))
               (cons (apply (first subst)
                            (append (rest subst)
                                    (list tr)))))))
      (loop :for substitutions :in substitution-passes
            :for tree* = (doit substitutions tree)
              :then (doit substitutions tree*)
            :do (progn) ; (print substitutions)
            :finally (return tree*)))))
