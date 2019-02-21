;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem :simplet
  :author "noloop <noloop@zoho.com>"
  :maintainer "noloop <noloop@zoho.com>"
  :license "GPLv3"
  :version "1.1.0"
  :homepage "https://github.com/noloop/simplet"
  :bug-tracker "https://github.com/noloop/simplet/issues"
  :source-control (:git "git@github.com:noloop/simplet.git")
  :description "Simple test runner in Common Lisp."
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "simplet" :depends-on ("package")))))
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "simplet/test"))))

(defsystem :simplet/test
  :author "noloop <noloop@zoho.com>"
  :maintainer "noloop <noloop@zoho.com>"
  :license "GPLv3"
  :description "simplet Test."
  :depends-on (:simplet)
  :components ((:module "test"
                :components
                ((:file "simplet-test"))))
  :perform (test-op (op system) (funcall (read-from-string "simplet-test::test-run"))))
