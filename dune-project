(lang dune 2.5)

(generate_opam_files true)

(version 0.1)

(license LGPL-3.0-or-later)
(authors "Niols <niols@niols.fr>")
(maintainers "Niols <niols@niols.fr>")
(source (github Niols/ppx_monad))

(package
 (name ppx_monad)
 (synopsis "A Syntax Extension for all Monadic Syntaxes")
 (depends ppxlib))
