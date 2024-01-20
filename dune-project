(lang dune 3.12)

(name forester_html)
(version a831375)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name forester_html)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune dream dream-html algaeff forester asai)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
