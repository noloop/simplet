# simplet

### _Simple test runner in Common Lisp._

## Getting Started in simplet

### Portability

### Dependencies

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

## Topics...

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

## API

function **(lib-function-name args)**

