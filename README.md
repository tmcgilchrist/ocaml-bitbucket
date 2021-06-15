OCaml Bitbucket
==========

This library provides an OCaml interface to the Bitbucket 2.0 API

It is not yet complete but lib/bitbucket.atd contains the data types that have been bound so far.

Development
----------

For development I suggest creating an opam switch for this project.

``` shell
# Setup isolated sandbox/switch
opam switch create 4.08.1-bitbucket 4.08.1

# Install deps
opam install --deps-only . -td

# Actually build
dune build

# Running tests
dune runtest
```

Copyright and license
----------
`ocaml-bitbucket` is distributed under the terms of the Berkeley Software Distribution license (3 clauses).


Documentation
----------

Excellent inline documentation examples:
https://carlosdagos.github.io/osh/osh/index.html

https://ocsigen.org/lwt
