open Cmdliner
open Ft_turing

let input =
  let doc = "input for the Turing Machine to process" in
  Arg.(value & pos 1 string "" & info [] ~docv:"input" ~doc)

let jsonfile =
  let doc = "json description of the Turing Machine" in
  Arg.(value & pos 0 string "" & info [] ~docv:"jsonfile" ~doc)

let usage =
  let doc = "Evaluation of Turing Machine descriptions defined in a JSON file." in
  let man =
    [ `S Manpage.s_bugs; `P "Email bug reports to <laradiopotin@gmail.com>." ]
  in
  Term.info "Ft_Turing" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

let interpret jsonfile _input =
  Format.printf "READING MACHINE FROM: %s@." jsonfile;
  let json = Yojson.Basic.from_file jsonfile in
  let machine = Lang.to_machine json in
  Pp.machine Format.std_formatter machine

let ft_turing = Term.(const interpret $ jsonfile $ input)

let () =
  Term.exit @@ Term.eval (ft_turing, usage)
