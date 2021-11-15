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
  Format.fprintf fmt "Test %d: %a" (test_number ()) comment com

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

let () =
  let fmt = Format.std_formatter in
  Format.fprintf fmt "LAUNCHING TESTS *********************@.";

  pp_test_nb fmt "checking machine description";
  let machine = get_machine fmt "test_machines/test_unary_sub_VALID.json" in
  compare_description machine unary_sub_definition;
  List.iter
    (fun (input, expected) ->
      pp_test_nb fmt (Format.sprintf {|VALID "%s"|} input);
      handle_test machine input expected;
      Format.print_flush () )
    unary_sub_valid_input;

  List.iter
    (fun (input, expected) ->
      pp_test_nb fmt (Format.sprintf {|INVALID "%s"|} input);
      handle_test machine input expected;
      Format.print_flush () )
    unary_sub_invalid_input;

  let machine = get_machine fmt "test_machines/unary_add.json" in
  compare_description machine unary_add_definition;
  List.iter
    (fun (input, expected) ->
      pp_test_nb fmt (Format.sprintf {|VALID "%s"|} input);
      handle_test machine input expected;
      Format.print_flush () )
    unary_add_valid_input;

  List.iter
    (fun (input, expected) ->
      pp_test_nb fmt (Format.sprintf {|INVALID "%s"|} input);
      handle_test machine input expected;
      Format.print_flush () )
    unary_add_invalid_input
