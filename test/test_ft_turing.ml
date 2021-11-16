open Ft_turing
open Machine_io

let trace expected got =
  Format.eprintf
    "@.********************* EXPECTED\t\t@.%s@.@.********************* GOT     \
     \t\t.@.%s@."
    expected got;
  exit 1

let comment fmt msg = Format.fprintf fmt "%s@." msg

let test_number =
  let count = ref (-1) in
  fun () ->
    incr count;
    !count

let pp_test_nb fmt com =
  Format.fprintf fmt "Test %02d: %a" (test_number ()) comment com

let get_machine fmt machinefile =
  let machine =
    match Lang.to_machine machinefile with
    | exception Utils.Error -> trace "a valid filename" machinefile
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
  let execution = Format.asprintf "%a" Execute.interpreter (machine, input) in
  try assert (String.equal execution expected) with
  | Assert_failure _ -> trace expected execution

let run_tests fmt machine kind input_list =
  List.iter
    (fun (input, expected) ->
      pp_test_nb fmt (Format.sprintf {|%s "%s"|} kind input);
      handle_test machine input expected;
      Format.print_flush () )
    input_list

let () =
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
  run_tests fmt machine "INVALID" zero_n_one_n_invalid_input
