(lang dune 3.17)

(name estoc)

(version 0.1.0)

(generate_opam_files true)

(source
 (github qexat/estoc))

(authors "lexa <contact@qexat.com>")

(license MIT)

(package
 (name estoc)
 (depends
  (ansifmt
   (>= 0.2))
  (ocaml
   (>= 5.3))))
