(in-package :compute)

(defun make-substitution (&key new rest consumed)
  (list :new new
        :consumed consumed
        :rest rest))

(defun substitutionp (substitution)
  (and (listp substitution)
       (eq (car substitution) :new)))

(defun substitution-new (substitution)
  (getf substitution :new))

(defun substitution-rest (substitution)
  (getf substitution :rest))

(defun substitution-consumed (substitution)
  (getf substitution :consumed))
