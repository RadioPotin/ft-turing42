open Ft_turing
open Test_utils

let get_machine fmt machinefile =
  Format.fprintf fmt "@.FILE: %s@." (Filename.basename machinefile);
  Lang.to_machine machinefile

let run_test fmt machinename =
  match get_machine fmt machinename with
  | exception Utils.Error -> ()
  | _machine -> assert false

let test_files =
  [ "INVALID_NAME.json"
  ; "INVALID_ACTION_FIELD.json"
  ; "INVALID_TYPE_STRING_LIST.json"
  ; "INVALID_TYPE_STRING_ASSOC.json"
  ; "INVALID_TYPE_LIST_INT.json"
  ; "INVALID_TYPE_LIST_ASSOC.json"
  ; "INVALID_TYPE_ASSOC_INT.json"
  ; "INVALID_TYPE_ASSOC_LIST.json"
  ; "INVALID_TYPE_TRANSITION_ASSOC.json"
  ; "INVALID_TYPE_TRANSITION_INT.json"
  ; "NOSUCHFILE.json"
  ]

let test_parser () =
  if Sys.os_type = "Win32" || Sys.os_type = "Cygwin" then
    ()
  else
    let fmt = Format.std_formatter in
    comment fmt "\nSTARTING PARSING TESTS **************";
    List.iter
      (fun machinename -> run_test fmt ("test_machines/" ^ machinename))
      test_files
