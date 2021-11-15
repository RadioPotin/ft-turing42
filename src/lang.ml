open Yojson

(** Type for action fields in transition definitions *)
type direction =
  | Left
  | Right

(** Type for transitions
    [to_state:string * write:string * direction: Left | Right ] *)
type transition = string * string * direction

(** [from_direction action] Function is used for printing and conversion of type
    [direction] to [string] *)
let from_direction = function
  | Right -> "RIGHT"
  | Left -> "LEFT"

(** [to_direction action] Function is used in parsing to encode action fields in
    transitions definitions *)
let to_direction = function
  | "RIGHT" -> Right
  | "LEFT" -> Left
  | action ->
    Utils.wrap_error Utils.error (Format.sprintf "Unknown action %s" action)

(** [to_string field json] extracts string date from [field] in a
    [Yojson.Basic.t] object [json]. Calls [Utils.err_invalid_type] if type is
    incorrect*)
let to_string field json =
  match Basic.Util.member field json with
  | `String s -> s
  | (`List _ | `Assoc _) as t ->
    Utils.wrap_error Utils.err_invalid_type true t field
  | t -> Utils.wrap_error Utils.err_invalid_type false t field

(** [to_string_option field json] extracts string list data from [field] in a
    [Yojson.Basic.t] object [json]. Calls [Utils.err_invalid_type] if type is
    incorrect *)
let to_string_list field json =
  match Basic.Util.member field json with
  | `List l ->
    List.map
      (fun s ->
        match Basic.Util.to_string_option s with
        | None -> Utils.wrap_error Utils.err_invalid_type false `Null field
        | Some s -> s )
      l
  | `Assoc _a as t -> Utils.wrap_error Utils.err_invalid_type true t field
  | t -> Utils.wrap_error Utils.err_invalid_type false t field

(** [to_assoc_knv_list field json] extracts a [(string * Yojson.Basic.t) list]
    from [field] that is contained in the [json] object. Calls
    [Utils.err_invalid_type] if type is incorrect *)
let to_assoc_knv_list field json =
  match Basic.Util.member field json with
  | `Assoc _a as t -> Yojson.Basic.Util.to_assoc t
  | `List _l as t -> Utils.wrap_error Utils.err_invalid_type true t field
  | t -> Utils.wrap_error Utils.err_invalid_type false t field

(** [get_transitions v] returns a
    [(read:string * (to_state:string * write:read * action:direction)) list]
    found in [Yojson.Basic.t] object [v]. Calls [Utils.err_invalid_type] if type
    is incorrect *)
let get_transitions = function
  | `List l ->
    List.map
      (fun l ->
        ( to_string "read" l
        , ( to_string "to_state" l
          , to_string "write" l
          , to_direction (to_string "action" l) ) ) )
      l
  | `Assoc _a as t ->
    Utils.wrap_error Utils.err_invalid_type true t "transitions"
  | t -> Utils.wrap_error Utils.err_invalid_type false t "transitions"

(** [to_transition_table alphabet states transitions] returns a
    [ (state:string * read:string) (to_state:string * write:read * action:direction) Hashtbl.t].
    This function also runs several kinds of sanity checks on the validity of
    transitions described in the json file *)
let to_transition_table alphabet states (transitions : (string * Basic.t) list)
    =
  let transitbl = Hashtbl.create 512 in
  List.iter
    (fun (state_key, v) ->
      List.iter
        (fun (read_key, (t : transition)) ->
          if not (List.mem read_key alphabet) then
            Utils.wrap_error Utils.error
              (Format.sprintf {|"read" value %s is undefined in %s|} read_key
                 state_key )
          else
            let collision = Hashtbl.find_opt transitbl (state_key, read_key) in
            match collision with
            | None ->
              Utils.assert_transition_ok transitbl t state_key read_key alphabet
                states
            | Some _transitionstate ->
              Utils.wrap_error Utils.error
                (Format.sprintf
                   {|Duplicate transition. State transition (%s) is already indexed somewhere in "transitions"@.|}
                   state_key ) )
        (get_transitions v) )
    transitions;
  transitbl

(** [to_transitions_tbl alphabet states field json] converts the value contained
    in [field] in the json object to [(string * Yojson.Basic.t) list] and feeds
    to the function handling the conversion to a type [Hashtbl.t] *)
let to_transitions_tbl alphabet states field = function
  | `Assoc _l as t ->
    to_assoc_knv_list field t |> to_transition_table alphabet states
  | `List _l as t -> Utils.wrap_error Utils.err_invalid_type true t field
  | t -> Utils.wrap_error Utils.err_invalid_type false t field

(** [to_states_tbl initial finals states] takes a [string list] of states, and
    converts it to a [string (is_initial:bool * is_final:bool) Hashtbl] and
    calls [Utils.error] if a given state appears several times in the "states"
    field in the json file *)
let to_states_tbl initial finals states =
  let tbl = Hashtbl.create 512 in
  List.iter
    (fun state ->
      let collision = Hashtbl.find_opt tbl state in
      match collision with
      | None ->
        Hashtbl.add tbl state (String.equal initial state, List.mem state finals)
      | Some (_init, _final) ->
        Utils.wrap_error Utils.error
          (Format.sprintf "State %s seems to be defined twice" state) )
    states;
  tbl

(** [get_value_and_check errmsg v check] simple generic function that takes an
    error message to feed to [Utils.error] if needed, a value [v] and a [check]
    function of type [(v:string -> bool)] *)
let get_value_and_check errmsg v check =
  if check v then
    v
  else
    Utils.wrap_error Utils.error (Format.sprintf "%s %s" errmsg v)

(** [convert_json jsonfile] takes [jsonfile] and converts it to a
    [Yojson.Basic.t] json object *)
let convert_json jsonfile =
  if Sys.file_exists jsonfile then
    Basic.from_file jsonfile
  else
    Utils.wrap_error Utils.error (Format.sprintf "Impossible to read file")

let get_name json jsonfile =
  let jsonfile = Filename.basename jsonfile in
  get_value_and_check "\"name\" field and filename are different:"
    (to_string "name" json)
    (String.equal (Filename.chop_extension jsonfile))

let get_alphabet json =
  let alphabet_field_value = to_string_list "alphabet" json in
  if List.for_all (fun s -> String.length s = 1) alphabet_field_value then
    alphabet_field_value
  else
    Utils.wrap_error Utils.error
      "Alphabet must be a list of strings of length equal to 1."

let get_blank json alphabet =
  get_value_and_check "Alphabet does not contain blank:"
    (to_string "blank" json) (fun v -> List.mem v alphabet)

let get_initial json states =
  get_value_and_check "States list does not contain initial state:"
    (to_string "initial" json) (fun v -> List.mem v states)

let get_final json states =
  let finals_field_value = to_string_list "finals" json in
  List.iter
    (fun final ->
      if List.mem final states then
        ()
      else
        Utils.wrap_error Utils.error
          (Format.sprintf "State %s is not a defined state" final) )
    finals_field_value;
  finals_field_value

(** [to_machnine jsonfile] function parses the jsonfile and does all necessary
    sanity checks and conversions for the interpreter *)
let to_machine jsonfile =
  let json = convert_json jsonfile in
  let name = get_name json jsonfile in
  (* Maybe check if an alphabet element is defined twice *)
  let alphabet = get_alphabet json in
  let blank = get_blank json alphabet in
  let states = to_string_list "states" json in
  let initial = get_initial json states in
  (* Maybe check if a final state is defined twice *)
  let finals = get_final json states in
  let states_tbl = to_states_tbl initial finals states in
  let transitions_tbl = to_transitions_tbl alphabet states "transitions" json in
  (name, alphabet, blank, states_tbl, initial, finals, transitions_tbl)
