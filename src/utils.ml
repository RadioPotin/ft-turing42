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

(** [assert_transition_ok transitbl t state_key read_key alphabet states]
    asserts a given transition is coherent and complies with specifications of
    turing machine definition. *)
let assert_transition_ok transitbl t state_key read_key states =
  let to_state, _write, _ = t in
  if not (List.mem to_state states) then
    error
      (Format.sprintf {|"to_state" value %s is undefined in transition %s|}
         to_state state_key )
  else
    Hashtbl.add transitbl (state_key, read_key) t
