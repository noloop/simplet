(in-package #:noloop.simplet)

;; TEST
(defun create-test (description fn)
  (lambda ()
    (list description (funcall fn))))

;; SUITE
(defun create-suite (description &rest tests)
  (lambda ()
    (let* ((test-results (mapcar
                          #'(lambda (i) (funcall i))
                          tests))
           (suite-result (every
                          #'(lambda (i) (equal t (cadr i)))
                          test-results)))
      (list description tests suite-result))))

;; RUNNER
(defun run-suites (suites)
  (let ((results
          (mapcar #'(lambda (i) (funcall i)) suites)))
    (print suites)
    (list results
          (every #'(lambda (i) (equal t (cddr i))) results))))

;; REPORTER
(defun reporter (runner-result)
  (let ((suite-results (car runner-result))
        (end-result (cdr runner-result)))
    (print runner-result)
    (dolist (suite suite-results)
      (dolist (test-results (cadr suite))
        (dolist (test test-results)
          (format t "~a: ~a~%" (car test) (cdr test))))
      (format t "~a: ~a~%~%" (car suite-results) (cddr suite-results)))
    (format t "Runner result: ~a~%~%" end-result)))

;; INTERFACE
(let ((suites '()))
  (defun test (description fn)
    (lambda ()
      (create-test description fn)))

  (defun suite (description &rest results)
    (push
     (lambda ()
       (create-suite description results))
     suites))

  (defun run ()
    (print suites)
    (reporter (funcall #'run-suites suites))))

#| Example of use:
CL-USER> (suite "Suite 1"
             (test "Test one equal one" #'(lambda () (= 1 1)))
             (test "Test two equal two" #'(lambda () (= 2 2))))

CL-USER> (run)
Test one equal one: T
Test two equal two: T
Suite 1: T

Runner result: T
|#

