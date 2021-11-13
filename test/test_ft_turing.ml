open Ft_turing

let unary_sub_valid_input =
  [ ( "1-1="
    , "[<1>-1=....] (scanright, 1) -> ( scanright, 1, RIGHT )\n\
       [1<->1=....] (scanright, -) -> ( scanright, -, RIGHT )\n\
       [1-<1>=....] (scanright, 1) -> ( scanright, 1, RIGHT )\n\
       [1-1<=>....] (scanright, =) -> ( eraseone, ., LEFT )\n\
       [1-<1>.....] (eraseone, 1) -> ( subone, =, LEFT )\n\
       [1<->=.....] (subone, -) -> ( skip, -, LEFT )\n\
       [<1>-=.....] (skip, 1) -> ( scanright, ., RIGHT )\n\
       [.<->=.....] (scanright, -) -> ( scanright, -, RIGHT )\n\
       [.-<=>.....] (scanright, =) -> ( eraseone, ., LEFT )\n\
       [.<->......] (eraseone, -) -> ( HALT, ., LEFT )\n" )
  ; ( "11-1="
    , "[<1>1-1=.....] (scanright, 1) -> ( scanright, 1, RIGHT )\n\
       [1<1>-1=.....] (scanright, 1) -> ( scanright, 1, RIGHT )\n\
       [11<->1=.....] (scanright, -) -> ( scanright, -, RIGHT )\n\
       [11-<1>=.....] (scanright, 1) -> ( scanright, 1, RIGHT )\n\
       [11-1<=>.....] (scanright, =) -> ( eraseone, ., LEFT )\n\
       [11-<1>......] (eraseone, 1) -> ( subone, =, LEFT )\n\
       [11<->=......] (subone, -) -> ( skip, -, LEFT )\n\
       [1<1>-=......] (skip, 1) -> ( scanright, ., RIGHT )\n\
       [1.<->=......] (scanright, -) -> ( scanright, -, RIGHT )\n\
       [1.-<=>......] (scanright, =) -> ( eraseone, ., LEFT )\n\
       [1.<->.......] (eraseone, -) -> ( HALT, ., LEFT )\n" )
  ; ( "1-="
    , "[<1>-=...] (scanright, 1) -> ( scanright, 1, RIGHT )\n\
       [1<->=...] (scanright, -) -> ( scanright, -, RIGHT )\n\
       [1-<=>...] (scanright, =) -> ( eraseone, ., LEFT )\n\
       [1<->....] (eraseone, -) -> ( HALT, ., LEFT )\n" )
  ]

let unary_sub_invalid_input =
  [ ( "\\"
    , "[<\\>.] (scanright, \\) -> BLOCKED\n\
       transition (scanright, \\) is undefined\n" )
  ; ( "="
    , "[<=>..] (scanright, =) -> ( eraseone, ., LEFT )\n\
       [<.>...] (eraseone, .) -> BLOCKED\n\
       transition (eraseone, .) is undefined\n" )
  ]

let trace expected got =
  Format.eprintf
    "********************* EXPECTED\t\t@.@.%s@.********************* GOT     \
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
  let machine = Lang.to_machine machinefile in
  Format.fprintf fmt "@.FILE: %s@.@." (Filename.basename machinefile);
  machine

let compare_description machine expected =
  let description = Format.asprintf "%a" Pp.machine machine in
  assert (String.equal description expected);
  ()

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

  let machine = get_machine fmt "test_machines/test_unary_sub_VALID.json" in
  pp_test_nb fmt "checking machine description";
  compare_description machine
    "********************************************************************************\n\n\
     *                                                                              \
     *\n\n\
     *                             \
     test_unary_sub_VALID                             *\n\n\
     *                                                                              \
     *\n\n\
     ********************************************************************************\n\
     Alphabet: [ 1, ., -, = ]\n\
     States  : [ scanright, eraseone, subone, HALT, skip ]\n\
     Initial : scanright\n\
     Finals  : [ HALT ]\n\
     (subone, -) -> ( skip, -, LEFT )\n\
     (subone, 1) -> ( subone, 1, LEFT )\n\
     (skip, .) -> ( skip, ., LEFT )\n\
     (skip, 1) -> ( scanright, ., RIGHT )\n\
     (scanright, =) -> ( eraseone, ., LEFT )\n\
     (scanright, .) -> ( scanright, ., RIGHT )\n\
     (scanright, 1) -> ( scanright, 1, RIGHT )\n\
     (scanright, -) -> ( scanright, -, RIGHT )\n\
     (eraseone, 1) -> ( subone, =, LEFT )\n\
     (eraseone, -) -> ( HALT, ., LEFT )\n\
     ********************************************************************************\n";

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
    unary_sub_invalid_input
