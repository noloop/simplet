(in-package #:cl-user)
(defpackage #:simplet-asdf
  (:nicknames #:simplet-asdf)
  (:use #:common-lisp
        #:asdf)
  (:export #:test-file))
(in-package #:simplet-asdf)

(defclass test-file (cl-source-file) ())
(defmethod operation-done-p ((op load-op) (c test-file)) nil)
(defmethod operation-done-p ((op compile-op) (c test-file)) t)

(import 'test-file :asdf)
