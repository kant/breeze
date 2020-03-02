
## Features

* Redefine `defun` to keep track of the definitions (before any macro expansion).
* A minimal test library that supports introspection.
* Workers - generic threads with message passing (thanks to [chanl](https://github.com/zkat/chanl)).
* Test-runner - a worker that runs tests (on demand).
* Can re-run all tests on function or test redifinition.

## Documentation

[Documentation](https://htmlpreview.github.io/?https://github.com/fstamour/breeze/blob/master/docs/index.html)

## Getting started

```lisp
(ql:quickload :breeze)
(in-package :breeze.user)
(br:main)
```

### Developping

```shell
grep -ir --include='*.lisp' todo
# or
rg -i todo
```

### To run the (self-) tests

```lisp
(ql:quickload 'breeze)
(load "tests/selftest.lisp")
(breeze.selftest:selftest)
```

### To generate the documentation

> See https://shinmera.github.io/staple/

```lisp
(staple:generate '#:breeze :if-exists :supersede)
```

### With [nix](https://nixos.org/)

```shell
# Will run sbcl and load breeze with quicklisp
./shell.nix
```

```shell
# Will update the documentation (docs/index.html)
./doc.nix
```

```shell
# Will run the tests
./test.nix
```

## Prior art

[Image based development](https://www.informatimago.com/develop/lisp/com/informatimago/small-cl-pgms/ibcl/index.html)
