(defpackage #:compute-with-arrows
  (:use #:cl)
  (:import-from #:alexandria
                #:curry
                #:rcurry)
  (:export #:cm
           #:compute
           #:defun-cm
           #:defmethod-cm
           #:access
           #:access-set))
