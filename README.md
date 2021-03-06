# simplet

### _Simple test runner in Common Lisp._

## Getting Started in simplet

### Portability

I believe it works in all implementations, but I only tested the one I use, which is the SBCL.

### Dependencies

No dependency.

### Download and load

**1 - Load simplet system by quicklisp**

```
(ql:quickload :simplet)
```

**2 - Download and load simplet system by github and asdf**

Download directly from github:

```
git clone https://github.com/noloop/simplet.git
```

and load by ASDF:

```lisp
(asdf:load-system :simplet)
```

_**Note: Remember to configure asdf to find your directory where you downloaded the libraries (asdf call them "systems") above, if you do not know how to make a read at: https://common-lisp.net/project/asdf/asdf/Configuring-ASDF-to-find-your-systems.html or https://lisp-lang.org/learn/writing-libraries.**_

## Create suite and test

```lisp
SIMPLET> (suite "Suite 1"
                (test "Test one equal one" #'(lambda () (= 1 1)))
                (test "Test two equal two" #'(lambda () (= 2 2))))
SIMPLET> (suite "Suite 2"
                (test "Test three equal three" #'(lambda () (= 3 3))))
```
## Run tests

```lisp
SIMPLET> (run)
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
SIMPLET> (run :return-string-p t)
NIL
```
## Suites and tests PENDING(or also called TODO)

It's simple to add a suite or test PENDING. To the suites, just do not add tests to it. To the tests just do not add a test function. Suites and tests PENDING do not make the runner result fail, however they are marked with PENDING instead of T. See the example below:

```lisp
SIMPLET> (suite "Suite 1"
                (test "Test one equal one" #'(lambda () (= 1 1)))
                (test "Test two equal two"))
SIMPLET> (suite "Suite 2")

SIMPLET> (run)
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
SIMPLET> (suite-only "Suite 1"
                     (test "Test one equal one" #'(lambda () (= 1 1)))
                     (test "Test two equal two" #'(lambda () (= 2 2))))
SIMPLET> (suite "Suite 2"
                (test "Test three equal three" #'(lambda () (= 3 3))))

SIMPLET> (run)
#...Simplet...#

Test one equal one: T
Test two equal two: T
Suite 1: T

Runner result: T

NIL
```

For tests only:

```lisp
SIMPLET> (suite "Suite 1"
                 (test-only "Test one equal one" #'(lambda () (= 1 1)))
                 (test "Test two equal two" #'(lambda () (= 2 2))))
SIMPLET> (suite "Suite 2"
                (test-only "Test three equal three" #'(lambda () (= 3 3))))

SIMPLET> (run)
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
SIMPLET> (suite-skip "Suite 1"
                     (test "Test one equal one" #'(lambda () (= 1 1)))
                     (test "Test two equal two" #'(lambda () (= 2 2))))
SIMPLET> (suite "Suite 2"
                 (test "Test three equal three" #'(lambda () (= 3 3))))

SIMPLET> (run)
#...Simplet...#

Test three equal three: T
Suite 2: T

Runner result: T

NIL
```

For tests skip:

```lisp
SIMPLET> (suite "Suite 1"
                 (test-skip "Test one equal one" #'(lambda () (= 1 1)))
                 (test "Test two equal two" #'(lambda () (= 2 2))))
SIMPLET> (suite "Suite 2"
                 (test-skip "Test three equal three" #'(lambda () (= 3 3))))

SIMPLET> (run)
#...Simplet...#

Test two equal two: T
Suite 1: T

Suite 2: T

Runner result: T

NIL
```

Beware of traps when mixing only and skip:

```lisp
SIMPLET> (suite-skip "Suite 1"
                     (test-only "Test one equal one" #'(lambda () (= 1 1)))
                     (test "Test two equal two" #'(lambda () (= 2 2))))
SIMPLET> (suite "Suite 2"
                 (test "Test three equal three" #'(lambda () (= 3 3))))

SIMPLET> (run)
#...Simplet...#

Runner result: T

NIL
```
## ASDF integration

Do not forget to add `:defsystem-depends-on (:simplet-asdf)` to  `your-app.asd` system definition file, this will enable`:test-file` in the `:components`. The class `:test-file` is similar to`:file` except it will be loaded only when call `asdf:test-system`. See an example below:

```lisp
(defsystem :your-app
  ;; ...
  :in-order-to ((test-op (test-op your-app/test))))

(defsystem :your-app/test
  :author "your <your@youremail.com>"
  :depends-on (:your-app :simplet)
  :defsystem-depends-on (:simplet-asdf)
  :components ((:module "test"
                :components
                ((:test-file "your-app-test"))))
  :perform (test-op (op c) (symbol-call :simplet '#:run)))
                           
```

To run tests with ASDF:

```lisp
(asdf:test-system :your-app)
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

function **(run &key return-string-p)**
