(lang dune 3.11)

(name hello)

(generate_opam_files true)

(source
 (github ricksterhd123/hellocaml))

(authors "ricksterhd123")

(license MIT)

(package
 (name hello)
 (description "Intro to ocaml")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
