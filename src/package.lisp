(defpackage #:noloop.simplet
  (:use #:common-lisp)
  (:nicknames #:simplet)
  (:import-from #:simplet-asdf
                #:test-file)
  (:export #:test
           #:test-only
           #:test-skip
	   #:suite
           #:suite-only
           #:suite-skip
           #:run))
