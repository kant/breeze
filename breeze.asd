
(asdf:defsystem #:breeze
  :name "breeze"
  :version "0"
  :maintainer "Francis St-Amour"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :description "A system to help automate work."
  :depends-on (#:uiop #:alexandria #:chanl #:anaphora)
  :serial t
  :components
  ((:module "src"
    :components
            ((:file "utils")
             (:file "definition")
             (:file "test")
             (:file "worker")
             (:file "test-runner")
             (:file "file-watcher")
             (:file "xref")
             (:file "asdf")
             (:file "user")))
   (:module "tests"
    :components
            ((:file "selftest")))))
