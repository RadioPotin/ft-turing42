(lang dune 2.8)

(using menhir 2.1)

(name ft_turing)

(authors "Dario Pinto <laradiopotin@gmail.com>")

(maintainers "Dario Pinto <laradiopotin@gmail.com>")

(source
 (github RadioPotin/ft-turing42))

(generate_opam_files true)

(package
 (name ft_turing)
 (synopsis "Turing machine interpreter")
 (description "An interpreter for Turing machines defined in .json files.")
 (depends
  (ocaml
   (>= 4.08))
  (dune
   (>= 2.8))
  (yojson
   (>= 1.7.0))
  (cmdliner
   (= 1.0.4))
  (containers
   (>= 3.5))
  (menhir
   (>= 2.1))
  (bisect_ppx
   (and
    :with-test
    (>= 2.6)
    :dev))))
