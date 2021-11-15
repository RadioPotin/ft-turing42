exception Error

(** [error msg] used for all matters of interruption of the program thoughout
    execution *)
let error msg =
  Format.eprintf "error: %s@." msg;
  raise Error

(** [err_invalid_type std t field] is called whenever an error of typing occurs
    during parsing of json file *)
let err_invalid_type std t field =
  Format.eprintf "error: Invalid type %s for field: \"%s\"@."
    (Yojson.Basic.pretty_to_string ~std t)
    field;
  raise Error

(** [wrap_error f]*)
let wrap_error f =
  try f with
  | Error -> Format.eprintf "EXITING@."; exit 1

(** [assert_transition_ok transitbl t state_key read_key alphabet states]
    asserts a given transition is coherent and complies with specifications of
    turing machine definition. *)
let assert_transition_ok transitbl t state_key read_key alphabet states =
  let to_state, write, _ = t in
  match (List.mem write alphabet, List.mem to_state states) with
  | false, false ->
    wrap_error error
      (Format.sprintf
         {|"to_state" and "write" values (%s, %s) are undefined in transition %s|}
         to_state write state_key )
  | false, true ->
    wrap_error error
      (Format.sprintf {|"write" value %s is undefined in transition %s|} write
         state_key )
  | true, false ->
    wrap_error error
      (Format.sprintf {|"to_state" value %s is undefined in transition %s|}
         to_state state_key )
  | true, true -> Hashtbl.add transitbl (state_key, read_key) t
