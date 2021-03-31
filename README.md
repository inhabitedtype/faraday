# Faraday

Faraday is a library for writing fast and memory-efficient serializers. Its
core type and related operation gives the user fine-grained control over
copying and allocation behavior while serializing user-defined types, and
presents the output in a form that makes it possible to use vectorized write
operations, such as the [writev][] system call, or any other platform or
application-specific output APIs.


[![Build Status](https://github.com/inhabitedtype/faraday/workflows/build/badge.svg)](https://github.com/inhabitedtype/faraday/actions?query=workflow%3A%22build%22)]

[writev]: http://man7.org/linux/man-pages/man2/writev.2.html

## Installation

Install the library and its depenencies via [OPAM][opam]:

[opam]: http://opam.ocaml.org/

```bash
opam install faraday
```

## Usage

Like its sister project [Angstrom][], Faraday is written with network protocols
and serialization formats in mind. As such, its source distribution inclues
implementations of various RFCs that are illustrative of real-world
applications of the library. This includes a [JSON serializer][json].

[angstrom]: https://github.com/inhabitedtype/angstrom
[json]: https://github.com/inhabitedtype/faraday/blob/master/examples/rFC7159.ml

In addition, it's appropriate here to include a serializer for the simple
arithmetic expression language described in Angstrom's README.

```ocaml
open Faraday

type 'a binop = [
  | `Sub of 'a * 'a
  | `Add of 'a * 'a
  | `Div of 'a * 'a
  | `Mul of 'a * 'a
]
;;

type t = [ `Num of int | t binop ]

let rec serialize ?(prec=0) t expr =
  match expr with
  | `Num n          -> write_string t (Printf.sprintf "%d" n)
  | #binop as binop ->
    let prec', op, l, r =
      match binop with
      | `Sub(l, r) -> 2, '-', l, r
      | `Add(l, r) -> 3, '+', l, r
      | `Div(l, r) -> 4, '/', l, r
      | `Mul(l, r) -> 5, '*', l, r
    in
    if prec' < prec then write_char t '(';
    serialize t ~prec:prec' l;
    write_char t op;
    serialize t ~prec:prec' r;
    if prec' < prec then write_char t ')'

let to_string expr =
  let t = create 0x1000 in
  serialize t expr;
  serialize_to_string t
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
the `alcotest` package:

```bash
opam install alcotest
make test
```

## License

BSD3, see LICENSE file for its text.
