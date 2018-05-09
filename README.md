OCaml Bitbucket
==========

This library provides an OCaml interface to the Bitbucket 2.0 API

It is not yet complete but lib/bitbucket.atd contains the data types that have been bound so far.

Development
----------

For development I suggest creating an opam switch for this project.

``` shell
# Setup isolated sandbox/switch
opam switch 4.04.2-bitbucket --alias 4.04.2

# Install deps
opam pin add -n bitbucket .
opam install --deps-only bitbucket -t

# Actually build
jbuilder build

# Running tests
jbuilder runtest
```

Copyright and license
----------
`ocaml-bitbucket` is distributed under the terms of the Berkeley Software Distribution license (3 clauses).
