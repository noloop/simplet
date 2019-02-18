(in-package #:noloop.simplet)

;; TEST
(defun create-test (description fn)
  (lambda ()
    (list description (funcall fn))))

;; SUITE
(defun create-suite (description &rest tests)
  (lambda ()
    (if (listp (car tests))
        (setf tests (car tests)))
    (let* ((test-results (mapcar
                          #'(lambda (i) (funcall i))
                          tests))
           (suite-result (every
                          #'(lambda (i) (equal t (cadr i)))
                          test-results)))
      (list description test-results suite-result))))

;; RUNNER
(defun run-suites (suites)
  (let ((results
          (mapcar #'(lambda (i) (funcall i)) suites)))
    (list results
          (every #'(lambda (i) (equal t (caddr i))) results))))

;; REPORTER
(defun reporter (runner-result &key (return-string nil))
  (let ((suite-results (car runner-result))
        (end-result (cadr runner-result))
        (epilogue ""))
    (dolist (suite suite-results)
      (dolist (test (cadr suite))
        (setf epilogue
              (concatenate 'string
                           epilogue
                           (format nil "~a: ~a~%" (car test) (cadr test)))))
      (setf epilogue
            (concatenate 'string
                         epilogue
                         (format nil "~a: ~a~%~%" (car suite) (caddr suite)))))
    (setf epilogue
          (concatenate 'string
                       epilogue
                       (format nil "Runner result: ~a~%~%" end-result)))
    (if return-string
        epilogue
        (format t "~a" epilogue))))

;; INTERFACE
(let ((suites '()))
  (defun get-suites ()
    suites)

  (defun clear-suites ()
    (setf suites '()))
  
  (defun test (description fn)
    (create-test description fn))

  (defun suite (description &rest tests)
    (push (create-suite description tests) suites))

  (defun run (&key (return-string nil))
    (if return-string
        (reporter (run-suites (nreverse suites)) :return-string t)
        (reporter (run-suites (nreverse suites))))))

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

