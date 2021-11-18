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
  ; "INVALID_TYPE_STRING_INT.json"
  ; "INVALID_TYPE_STRING_LIST.json"
  ; "INVALID_TYPE_STRING_ASSOC.json"
  ; "INVALID_TYPE_LIST_INT.json"
  ; "INVALID_TYPE_LIST_NULL.json"
  ; "INVALID_TYPE_LIST_ASSOC.json"
  ; "INVALID_TYPE_ASSOC_INT.json"
  ; "INVALID_TYPE_ASSOC_LIST.json"
  ; "INVALID_TYPE_TRANSITION_ASSOC.json"
  ; "INVALID_TYPE_TRANSITION_ASSOC_INT.json"
  ; "INVALID_TYPE_TRANSITION_ASSOC_LIST.json"
  ; "INVALID_DUPLICATE_STATE_DECLARATION.json"
  ; "INVALID_DUPLICATE_TRANSITION.json"
  ; "INVALID_TOSTATE_FIELD.json"
  ; "INVALID_FINAL_STATE.json"
  ; "INVALID_ALPHABET.json"
  ; "NOSUCHFILE.json"
  ]

let non_assoc_json =
  [ "INVALID_TYPE_MACHINE_INT.json"; "INVALID_TYPE_MACHINE_LIST.json" ]

let test_parser () =
  if Sys.os_type = "Win32" || Sys.os_type = "Cygwin" then
    ()
  else
    let fmt = Format.std_formatter in
    comment fmt "\nSTARTING PARSING TESTS **************";
    List.iter
      (fun machinename -> run_test fmt ("test_machines/" ^ machinename))
      test_files;

    List.iter
      (fun machinename ->
        Format.fprintf fmt "@.FILE: %s@." (Filename.basename machinename);
        match
          Lang.to_transitions_tbl [ "" ] "jsonfile"
            (Lang.convert_json ("test_machines/" ^ machinename))
        with
        | exception Utils.Error -> ()
        | _any -> assert false )
      non_assoc_json
