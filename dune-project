(lang dune 3.10)

(name huffman)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Pierre Villemot")

(maintainers "Pierre Villemot")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name huffman)
 (allow_empty)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   ocaml
   dune
   fmt
   (qcheck :with-test))
 (tags
  (topics "to describe" your project)))
