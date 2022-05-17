(lang dune 3.0)

(name ocaml-ast-to-json)

(implicit_transitive_deps false)

(generate_opam_files true)

(license "LGPL-2.1-only")

(authors "Marc-Antoine Ouimet")

(maintainers "ouimetmarcantoine@gmail.com")

(homepage "https://github.com/MartyO256/ocaml-ast-to-json/")

(source
 (github "MartyO256/OCaml-AST-to-JSON"))

(package
 (name ocaml-ast-to-json)
 (synopsis
  "Utility for parsing and converting OCaml AST to JSON using `compiler-libs`")
 (depends
  ocaml
  (ocamlformat :dev)
  (ocaml-lsp-server :dev)
  ))
