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
