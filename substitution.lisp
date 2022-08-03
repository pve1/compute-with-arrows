(in-package :compute-with-arrows)

(defun make-substitution (&key new rest consumed type)
  (list 'substitution
        :new new
        :type type
        :consumed consumed
        :rest rest))

(defun substitutionp (substitution)
  (and (listp substitution)
       (eq (car substitution) 'substitution)))

(defun substitution-new (substitution)
  (getf (cdr substitution) :new))

(defun substitution-rest (substitution)
  (getf (cdr substitution) :rest))

(defun substitution-consumed (substitution)
  (getf (cdr substitution) :consumed))

(defun substitution-type (substitution)
  (getf (cdr substitution) :type))
