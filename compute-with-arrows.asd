(asdf:defsystem #:compute-with-arrows
  :description "Describe compute-with-arrows here"
  :author "Peter von Etter"
  :license  "LLGPL"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "util")
               (:file "parse-destructuring-form")
               (:file "substitution")
               (:file "perform-substitutions")
               (:file "compute")
               (:file "def-cm")))
