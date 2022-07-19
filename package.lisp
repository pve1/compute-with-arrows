(defpackage #:compute-with-arrows
  (:use #:cl)
  (:import-from #:alexandria
                #:curry
                #:rcurry)
  (:export #:compute
           #:cm
           #:cm1
           #:defun-cm
           #:defmethod-cm))
