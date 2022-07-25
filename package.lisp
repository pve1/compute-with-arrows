(defpackage #:compute-with-arrows
  (:use #:cl)
  (:import-from #:alexandria
                #:curry
                #:rcurry)
  (:export #:cm
           #:cm1
           #:compute
           #:defun-cm
           #:defmethod-cm
           #:access
           #:access-set))
