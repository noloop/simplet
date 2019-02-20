(in-package #:cl-user)
(defpackage #:simplet-asdf
  (:nicknames #:prove-asdf)
  (:use #:common-lisp
        #:asdf)
  (:export #:test-file
           #:run-simplet-asdf))
(in-package #:simplet-asdf)

(defvar *system-test-files* (make-hash-table))

(defclass test-file (asdf:cl-source-file) ())

(defmethod asdf:perform ((op asdf:compile-op) (c test-file))
  ;; do nothing
  )

(defmethod asdf:perform ((op asdf:load-op) (c test-file))
  (pushnew c (gethash (asdf:component-system c) *system-test-files*)
           :key #'asdf:component-pathname
           :test #'equal))

(defun run-simplet-asdf (system-designator)
  "Runs a testing ASDF system."
  #+quicklisp (ql:quickload (if (typep system-designator 'asdf:system)
                                (asdf:component-name system-designator)
                                system-designator))
  #-quicklisp (asdf:load-system system-designator)
  (restart-case
      (dolist (c (reverse
                  (gethash (asdf:find-system system-designator) *system-test-files*)))
        (restart-case
            (asdf:perform 'asdf:load-source-op c)))))

(import 'test-file :asdf)
