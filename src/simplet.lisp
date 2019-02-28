(in-package #:noloop.simplet)

;; TEST
(defun create-test (description fn &key only skip)
  (lambda ()
    (list description
          (if (null fn)
              "PENDING"
              (funcall fn))
          only skip)))

;; SUITE
(defun create-suite (description tests &key only skip)
  (lambda ()
    (if (null (car tests))
        (list description '() "PENDING")
        (create-list-suite-result description tests only skip))))

(defun create-list-suite-result (description tests only skip)
  (let* ((test-results (mapcar #'(lambda (i) (funcall i)) tests))
         (suite-result (every
                        #'(lambda (i) (or (equal t (cadr i))
                                          (equalp "PENDING" (cadr i))))
                        test-results))
         (tests-only (collect-tests-only test-results)))
    (if tests-only
        (progn (setf test-results tests-only)
               (setf only t)))
    (setf test-results (remove-if #'cadddr test-results))
    (list description test-results suite-result only skip)))

;; RUNNER
(defun run-suites (suites)
  (let* ((suite-results
           (mapcar #'(lambda (i) (funcall i)) suites))
         (suites-only (collect-suites-only suite-results)))
    (if suites-only
        (setf suite-results suites-only))
    (setf suite-results (remove-if #'(lambda (i)
                                       (if (cadr (cdddr i))
                                           i))
                                   suite-results))
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
(defun reporter (runner-result &key return-string-p)
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
    (if return-string-p
        (format nil "#...Simplet...#~%~%~a" epilogue)
        (format t "#...Simplet...#~%~%~a" epilogue))))

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

  (defun test-skip (description &optional (fn nil))
    (create-test description fn :skip t))

  (defun suite (description &rest tests)
    (push (create-suite description tests) suites))

  (defun suite-only (description &rest tests)
    (push (create-suite description tests :only t) suites))

  (defun suite-skip (description &rest tests)
    (push (create-suite description tests :skip t) suites))

  (defun run (&key return-string-p)
    (let ((runner-result (run-suites (nreverse suites))))
      (clear-suites)
      (if return-string-p
          (reporter runner-result :return-string-p t)
          (reporter runner-result)))))

