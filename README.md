# Faraday

Faraday is a serialization library that makese it easy to write efficient and
reusable serializers suitable for high-performance applications.

## Installation

Install the library and its depenencies via [OPAM][opam]:

[opam]: http://opam.ocaml.org/

```bash
opam install angstrom
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
