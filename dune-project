(lang dune 3.12)

(name ocaml_fx)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ocaml_fx)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune dream dream-html algaeff forester asai)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
