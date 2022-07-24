(in-package :compute-with-arrows)

(defgeneric access (object property))
(defgeneric access-set (object property new))
(defsetf access %set-access)
(defmacro %set-access (object property new)
  `(progn
     (setf ,object (access-set ,object ,property ,new))
     ,new))

(defmethod access ((object standard-object) property)
  (slot-value object property))

(defmethod access ((object list) property)
  (if (consp (car object))
      (alexandria:assoc-value object property)
      (getf object property)))

(defmethod access ((object hash-table) property)
  (gethash property object))

(defmethod access ((object vector) property)
  (aref object property))

(defmethod access-set ((object standard-object) property new)
  (setf (slot-value object property) new)
  object)

(defmethod access-set ((object list) property new)
  (if (consp (car object))
      (let ((old (assoc property object)))
        (if old
            (progn
              (setf (cdr old) new)
              object)
            (cons (cons property new) object)))
      (progn
        (setf (getf object property) new)
        object)))

(defmethod access-set ((object hash-table) property new)
  (setf (gethash property object) new)
  object)

(defmethod access-set ((object vector) property new)
  (setf (aref object property) new)
  object)
