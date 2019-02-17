(in-package #:cl-user)
(defpackage #:noloop.simplet-test
  (:use #:common-lisp)
  (:nicknames #:simplet-test)
  (:import-from #:simplet
                #:create-test
                #:create-suite))
(in-package #:noloop.simplet-test)

(defun run ()
  (test "Test create-test" #'test-create-test)
  (test "Test suite-test" #'test-create-suite))

(defun test (stg test-fn)
  (let ((result (funcall test-fn)))
    (format t "~a: ~a~%" stg result)
    result))

(defun suite (&rest results)
  (format t "Tests result: ~a~%~%"
          (every #'(lambda (el) (equal t el)) results)))

(defun fix-passing-test (num)
  (create-test
   (concatenate 'string "Test-" (string num))
   #'(lambda () (= 1 1))))

(defun test-create-test ()
  (let* ((test-1 (fix-passing-test "1"))
         (list-test-1 (funcall test-1)))
    (and (string= "Test-1" (car list-test-1))
         (cadr list-test-1))))

(defun test-create-suite ()
  (let* ((suite-1 (create-suite "Suite-1"
                           (fix-passing-test "1")
                           (fix-passing-test "2")))
         (list-suite-1 (funcall suite-1)))
    (and (string= "Suite-1" (car list-suite-1))
         (caddr list-suite-1))))
