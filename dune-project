(lang dune 2.7)

(generate_opam_files true)

(version 0.2.0)

(license LGPL-3.0-or-later)
(authors "Niols <niols@niols.fr>")
(maintainers "Niols <niols@niols.fr>")
(source (github Niols/ppx_monad))

(name ppx_monad)

(package
 (name ppx_monad)
 (synopsis "A Syntax Extension for all Monadic Syntaxes")
 (depends
  (ocaml (>= 4.08.0))
   dune-configurator
  (ppxlib (>= 0.9.0))))
