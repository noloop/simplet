# simplet

### _Simple test runner in Common Lisp._

## Getting Started in simplet

### Portability

I believe it works in all implementations, but I only tested the one I use, which is the SBCL.

### Dependencies

No dependency.

### Download and installation

**1 - Download simplet system**

By quicklisp:

```
IN PROGRESS...
```

or directly from github:

```
git clone https://github.com/noloop/simplet.git
```
**2 - Install simplet**

By quicklisp:

```
IN PROGRESS...
```

or directly from asdf:

```lisp
(asdf:load-system :simplet)
```

_**Note: Remember to configure asdf to find your directory where you downloaded the libraries (asdf call them "systems") above, if you do not know how to make a read at: https://common-lisp.net/project/asdf/asdf/Configuring-ASDF-to-find-your-systems.html or https://lisp-lang.org/learn/writing-libraries.**_

## Create suite and test

```lisp
CL-USER> (suite "Suite 1"
                (test "Test one equal one" #'(lambda () (= 1 1)))
                (test "Test two equal two" #'(lambda () (= 2 2))))
CL-USER> (suite "Suite 2"
                (test "Test three equal three" #'(lambda () (= 3 3))))
```
## Run tests

```lisp
CL-USER> (run)
#...Simplet...#

Test one equal one: T
Test two equal two: T
Suite 1: T

Test three equal three: T
Suite 2: T

Runner result: T

NIL
```

You also can getting an string of run, instead of printing on REPL:

```lisp
CL-USER> (run :return-string t)
NIL
```
## Suites and tests PENDING(or also called TODO)

It's simple to add a suite or test PENDING. To the suites, just do not add tests to it. To the tests just do not add a test function. Suites and tests PENDING do not make the runner result fail, however they are marked with PENDING instead of T. See the example below:

```lisp
CL-USER> (suite "Suite 1"
                (test "Test one equal one" #'(lambda () (= 1 1)))
                (test "Test two equal two"))
CL-USER> (suite "Suite 2")

CL-USER> (run)
#...Simplet...#

Test one equal one: T
Test two equal two: PENDING
Suite 1: T

Suite 2: PENDING

Runner result: T

NIL
```

## Suites and tests only/skip

For suites only:

```lisp
CL-USER> (suite-only "Suite 1"
                     (test "Test one equal one" #'(lambda () (= 1 1)))
                     (test "Test two equal two" #'(lambda () (= 2 2))))
CL-USER> (suite "Suite 2"
                (test "Test three equal three" #'(lambda () (= 3 3))))

CL-USER> (run)
#...Simplet...#

Test one equal one: T
Test two equal two: T
Suite 1: T

Runner result: T

NIL
```

For tests only:

```lisp
CL-USER> (suite "Suite 1"
                 (test-only "Test one equal one" #'(lambda () (= 1 1)))
                 (test "Test two equal two" #'(lambda () (= 2 2))))
CL-USER> (suite "Suite 2"
                (test-only "Test three equal three" #'(lambda () (= 3 3))))

CL-USER> (run)
#...Simplet...#

Test one equal one: T
Suite 1: T

Test three equal three: T
Suite 2: T

Runner result: T

NIL
```

For suites skip:

```lisp
CL-USER> (suite-skip "Suite 1"
                     (test "Test one equal one" #'(lambda () (= 1 1)))
                     (test "Test two equal two" #'(lambda () (= 2 2))))
CL-USER> (suite "Suite 2"
                 (test "Test three equal three" #'(lambda () (= 3 3))))

CL-USER> (run)
#...Simplet...#

Test three equal three: T
Suite 2: T

Runner result: T

NIL
```

For tests skip:

```lisp
CL-USER> (suite "Suite 1"
                 (test-skip "Test one equal one" #'(lambda () (= 1 1)))
                 (test "Test two equal two" #'(lambda () (= 2 2))))
CL-USER> (suite "Suite 2"
                 (test-skip "Test three equal three" #'(lambda () (= 3 3))))

CL-USER> (run)
#...Simplet...#

Test two equal two: T
Suite 1: T

Suite 2: T

Runner result: T

NIL
```

Beware of traps when mixing only and skip:

```lisp
CL-USER> (suite-skip "Suite 1"
                     (test-only "Test one equal one" #'(lambda () (= 1 1)))
                     (test "Test two equal two" #'(lambda () (= 2 2))))
CL-USER> (suite "Suite 2"
                 (test "Test three equal three" #'(lambda () (= 3 3))))

CL-USER> (run)
#...Simplet...#

Runner result: T

NIL
```
## ASDF integration

Do not forget to add `:defsystem-depends-on (:simplet-asdf)` to  `your-app.asd` system definition file, this will enable`:test-file` in the `: components`. The class `:test-file` is similar to`:file` except it will be loaded only when call `asdf:test-system`. See an example below:

```lisp
(defsystem :your-app
  ;; ...
  :in-order-to ((test-op (test-op my-app-test))))

(defsystem :skeleton-creator/test
  :author "your <your@youremail.com>"
  :depends-on (:your-app :simplet)
  :defsystem-depends-on (:simplet-asdf)
  :components ((:module "test"
                :components
                ((:test-file "your-app-test"))))
  :perform (test-op :after (op c)
                    (progn (funcall (intern #.(string :run-simplet-asdf) :simplet) c)
                           (symbol-call :simplet '#:run))))
                           
```

## Limitations

* Whitout cacth error
* Solely synchronous tests
* Whitout fixtures
* Whitout timeout
* Without recursives suites
* Without assertion libraries integration
* Not possible execution of tests whitout suites(because not contain the concept of root-suite.)

## API

function **(suite description &rest tests)**

function **(suite-only description &rest tests)**

function **(suite-skip description &rest tests)**

function **(test description &optional fn)**

function **(test-only description &optional fn)**

function **(test-skip description &optional fn)**

function **(run &key return-string)**
