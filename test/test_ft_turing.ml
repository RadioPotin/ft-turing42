open Ft_turing
open Machine_io
open Test_utils

let get_machine fmt machinefile =
  let machine =
    match Lang.to_machine machinefile with
    | exception Utils.Error -> exit 1
    | machine -> machine
  in
  Format.fprintf fmt "@.FILE: %s@.@." (Filename.basename machinefile);
  machine

let compare_description machine expected =
  let description = Format.asprintf "%a" Pp.machine machine in
  try assert (String.equal description expected) with
  | Assert_failure _ -> trace expected description

let handle_test
    (_name, alphabet, blank, states_tbl, initial, _finals, transitions_tbl)
    input expected =
  let machine = (alphabet, blank, initial, (states_tbl, transitions_tbl)) in
  let execution =
    match Format.asprintf "%a" Execute.interpreter (machine, input) with
    | exception Utils.Error -> exit 1
    | s -> s
  in
  try assert (String.equal execution expected) with
  | Assert_failure _ -> trace expected execution

let run_tests fmt machine kind input_list =
  List.iter
    (fun (input, expected) ->
      pp_test_nb fmt (Format.sprintf {|%s "%s"|} kind input);
      handle_test machine input expected;
      Format.print_flush () )
    input_list

let handle_input_test
    (_name, alphabet, blank, states_tbl, initial, _finals, transitions_tbl)
    input =
  let machine = (alphabet, blank, initial, (states_tbl, transitions_tbl)) in
  match Format.asprintf "%a" Execute.interpreter (machine, input) with
  | exception Utils.Error -> ()
  | _s -> assert false

let run_input_tests fmt machine kind input_list =
  List.iter
    (fun input ->
      pp_test_nb fmt (Format.sprintf {|%s "%s"|} kind input);
      handle_input_test machine input;
      Format.print_flush ();
      print_newline () )
    input_list

let () =
  if Sys.os_type = "Win32" || Sys.os_type = "Cygwin" then
    ()
  else
    let fmt = Format.std_formatter in
    Format.fprintf fmt "LAUNCHING TESTS *********************@.";

    let machine = get_machine fmt "test_machines/test_unary_sub_VALID.json" in
    pp_test_nb fmt "checking machine description";
    compare_description machine unary_sub_definition;
    run_tests fmt machine "VALID" unary_sub_valid_input;
    run_tests fmt machine "INVALID" unary_sub_invalid_input;

    let machine = get_machine fmt "test_machines/unary_add.json" in
    pp_test_nb fmt "checking machine description";
    compare_description machine unary_add_definition;
    run_tests fmt machine "VALID" unary_add_valid_input;
    run_tests fmt machine "INVALID" unary_add_invalid_input;

    let machine = get_machine fmt "test_machines/02n.json" in
    pp_test_nb fmt "checking machine description";
    compare_description machine zero_two_n_definition;
    run_tests fmt machine "VALID" zero_two_n_valid_input;
    run_tests fmt machine "INVALID" zero_two_n_invalid_input;

    let machine = get_machine fmt "test_machines/palindrome.json" in
    pp_test_nb fmt "checking machine description";
    compare_description machine palindrome_definition;
    run_tests fmt machine "VALID" palindrome_valid_input;
    run_tests fmt machine "INVALID" palindrome_invalid_input;

    let machine = get_machine fmt "test_machines/0n1n.json" in
    pp_test_nb fmt "checking machine description";
    compare_description machine zero_n_one_n_definition;
    run_tests fmt machine "VALID" zero_n_one_n_valid_input;
    run_tests fmt machine "INVALID" zero_n_one_n_invalid_input;

    let machine = get_machine fmt "test_machines/VALID_MULTIPLE_FINALS.json" in
    pp_test_nb fmt "checking machine description";
    compare_description machine valid_multiple_finals_definition;

    let machine = get_machine fmt "test_machines/unary_sub.json" in
    Format.fprintf fmt "INVALID INPUT TESTS *****************@.@.";
    run_input_tests fmt machine "INVALID INPUT" invalid_input;

    Test_parser.test_parser ()
