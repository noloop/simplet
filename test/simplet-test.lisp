(in-package #:cl-user)
(defpackage #:noloop.simplet-test
  (:use #:common-lisp)
  (:nicknames #:simplet-test)
  (:import-from #:simplet
                #:create-test
                #:create-suite
                #:run-suites
                #:reporter))
(in-package #:noloop.simplet-test)

(defun run ()
  (suite
   (test "Test create-test" #'test-create-test)
   (test "Test suite-test" #'test-create-suite)
   (test "Test run-suites" #'test-run-suites)
   (test "Test reporter" #'test-reporter)))

(defun test (stg test-fn)
  (let ((result (funcall test-fn)))
    (format t "~a: ~a~%" stg result)
    result))

(defun suite (&rest results)
  (format t "~%Tests result: ~a~%~%"
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
         (list-suite-1 (funcall suite-1))
         (suite-description (car list-suite-1))
         (suite-tests (cadr list-suite-1))
         (suite-result (caddr list-suite-1)))
    (and (string= "Suite-1" suite-description)
         (= 2 (length suite-tests))
         suite-result)))

(defun test-run-suites ()
  (let* ((suite-1 (create-suite "Suite-1"
                                (fix-passing-test "1")
                                (fix-passing-test "2")))
         (suite-2  (create-suite "Suite-2"
                                 (fix-passing-test "1")
                                 (fix-passing-test "2")))
         (suites (list suite-1 suite-2))
         (runner-result (run-suites suites)))
    (and (cadr runner-result))))

(defun test-reporter ()
  (let* ((suite-1 (create-suite "Suite-1"
                                (fix-passing-test "1")
                                (fix-passing-test "2")))
         (suite-2  (create-suite "Suite-2"
                                 (fix-passing-test "1")
                                 (fix-passing-test "2")))
         (suites (list suite-1 suite-2))
         (runner-result (run-suites suites))
         (expected-stg (format nil "~a~%~a~%~a~%~a~%~a~%~a~%~a~%~a~%~a~%~a~%"
                               "Test-1: T"
                               "Test-2: T"
                               "Suite-1: T"
                               ""
                               "Test-1: T"
                               "Test-2: T"
                               "Suite-2: T"
                               ""
                               "Runner result: T"
                               ""))
         (actual-stg ""))
    (setf actual-stg (reporter runner-result :return-string t))
    (string= expected-stg actual-stg)))


