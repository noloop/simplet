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
    (reporter (run-suites suites))))

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

