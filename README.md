# Faraday

Faraday is a library of low-level primitives for writing serializers for
user-defined datatypes. Its primitives provide the user fine-grained control
over copying and allocation behavior, and presents serialization output in a
form that is suitable for use with vectorized writes via the [writev][] system
call or any other platform or application-specific output interfaces.

[![Build Status](https://travis-ci.org/inhabitedtype/faraday.svg?branch=master)](https://travis-ci.org/inhabitedtype/faraday)

[writev]: http://man7.org/linux/man-pages/man2/writev.2.html

## Installation

Install the library and its depenencies via [OPAM][opam]:

[opam]: http://opam.ocaml.org/

```bash
opam install faraday
```

## Development

To install development dependencies, pin the package from the root of the
repository:

```bash
opam pin add -n faraday .
opam install --deps-only faraday
```

After this, you may install a development version of the library using the
install command as usual.

For building and running the tests during development, you will need to install
the `alcotest` package and reconfigure the build process to enable tests:

```bash
opam install alcotest
./configure --enable-tests
make && make test
```

## License

BSD3, see LICENSE file for its text.
