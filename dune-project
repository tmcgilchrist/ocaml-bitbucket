(lang dune 1.10)

(name bitbucket)

(generate_opam_files true)

(license BSD-3-clause)
(maintainers "Tim McGilchrist <timmcgil@gmail.com>")
(authors "Tim McGilchrist" )
(source (github tmcgilchrist/ocaml-bitbucket))
(documentation "https://tmcgilchrist.github.io/ocaml-bitbucket/")

(package
 (name bitbucket)
 (depends
  (ocaml (>= 4.15))
  (dune (>= 1.10))
  (uri (>= 1.9.0))
  (cohttp (>= 0.99.0))
  (cohttp-lwt (>= 0.99))
  (lwt (>= 2.4.4))
  (atdgen (>= 2.0.0))
  (yojson (>= 1.6.0))
  stringext)
 (synopsis "Bitbucket APIv2 OCaml library")
 (description "This library provides an OCaml interface to the
[Bitbucket APIv2](https://developer.atlassian.com/bitbucket/api/2/reference/) (JSON).

It is compatible with [MirageOS](https://mirage.io) and also compiles to pure
JavaScript via [js_of_ocaml](http://ocsigen.org/js_of_ocaml)."))

(package
 (name bitbucket-unix)
 (depends
  (ocaml (>= 4.08))
  (dune (>= 1.10))
  (bitbucket (= :version))
  (cohttp (>= 0.99.0))
  (cohttp-lwt-unix (>= 0.99.0))
  stringext
  (cmdliner (>= 0.9.8))
  base-unix)
 (synopsis "Bitbucket APIv2 OCaml library")
 (description "This library provides an OCaml interface to the
[Bitbucket APIv2](https://developer.atlassian.com/bitbucket/api/2/reference/) (JSON).
This package installs the Unix (Lwt) version."))
