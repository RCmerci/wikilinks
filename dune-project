(lang dune 1.11)
(name wiki-link)

(generate_opam_files true)

(license MIT)
(maintainers "rcmerci <rcmerci@gmail.com>")
(authors "rcmerci <rcmerci@gmail.com>")
(source (github RCmerci/tdbooster))

(package
 (name wiki-link)
 (depends
  (ocaml (>= 4.10))
  core_kernel
  js_of_ocaml-lwt
  lwt)
  (synopsis "wiki link"))