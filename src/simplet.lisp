(in-package #:noloop.simplet)

;; TEST
(defun create-test (description fn &key (only nil))
  (lambda ()
    (list description
          (if (null fn)
              "PENDING"
              (funcall fn))
          only)))

;; SUITE
(defun create-suite (description tests &key (only nil))
  (lambda ()
    (if (null (car tests))
        (list description '() "PENDING")
        (create-list-suite-result description tests only))))

(defun create-list-suite-result (description tests only)
  (let* ((test-results (mapcar #'(lambda (i) (funcall i)) tests))
         (suite-result (every
                        #'(lambda (i) (or (equal t (cadr i))
                                          (equalp "PENDING" (cadr i))))
                        test-results))
         (tests-only (collect-tests-only test-results)))
    (if tests-only
        (progn (setf test-results tests-only)
               (setf only t)))
    (list description test-results suite-result only)))

;; RUNNER
(defun run-suites (suites)
  (let* ((suite-results
           (mapcar #'(lambda (i) (funcall i)) suites))
         (suites-only (collect-suites-only suite-results)))
    (if suites-only
        (setf suite-results suites-only))
    (list suite-results
          (every #'(lambda (i) (or (equal t (caddr i))
                                   (equalp "PENDING" (caddr i))))
                 suite-results))))

(defun collect-suites-only (suites)
  (remove nil (mapcar #'(lambda (i) (if (cadddr i) i))
                      suites)))

(defun collect-tests-only (tests)
  (remove nil (mapcar #'(lambda (i) (if (caddr i) i))
                      tests)))

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
        (format t "Simplet...~%~%~a" epilogue))))

;; INTERFACE
(let ((suites '()))
  (defun get-suites ()
    suites)

  (defun clear-suites ()
    (setf suites '()))
  
  (defun test (description &optional (fn nil))
    (create-test description fn))

  (defun test-only (description &optional (fn nil))
    (create-test description fn :only t))

  (defun suite (description &rest tests)
    (push (create-suite description tests) suites))

  (defun suite-only (description &rest tests)
    (push (create-suite description tests :only t) suites))

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

