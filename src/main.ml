open Cmdliner
open Ft_turing

let input =
  let doc = "input for the Turing Machine to process" in
  Arg.(value & pos 1 string "" & info [] ~docv:"input" ~doc)

let jsonfile =
  let doc = "json description of the Turing Machine" in
  Arg.(value & pos 0 string "" & info [] ~docv:"jsonfile" ~doc)

let usage =
  let doc =
    "Parsing, evaluation and execution of Turing Machine descriptions defined \
     in a JSON file."
  in
  let man =
    [ `S Manpage.s_bugs; `P "Email bug reports to <laradiopotin@gmail.com>." ]
  in
  Term.info "Ft_Turing" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits
    ~man

let main jsonfile input =
  let ( (_name, alphabet, blank, states_tbl, initial, _finals, transitions_tbl)
      as machine ) =
    Lang.to_machine jsonfile
  in
  Pp.machine Format.std_formatter machine;
  let machine = (alphabet, blank, initial, states_tbl, transitions_tbl) in
  Execute.interpreter machine input

let ft_turing = Term.(const main $ jsonfile $ input)

let () = Term.exit @@ Term.eval (ft_turing, usage)
