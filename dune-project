(lang dune 3.12)

(name dream_auth)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name dream_auth)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune dream (dream-html (= 3.0.0)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
