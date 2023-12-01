(lang dune 3.11)

(name aoc_2023_ocaml)

(generate_opam_files true)

(source
 (github laurentiu-dancu/aoc-2023-ocaml))

(authors "Laurentiu Dancu")

(license sandia-mit)

(package
 (name aoc_2023_ocaml)
 (synopsis "AoC 2023 in ocaml")
 (description "Advent of Code 2023 for learning ocaml")
 (depends ocaml dune)
 (tags
  (aoc-2023)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
