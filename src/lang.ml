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
  | action -> Utils.error (Format.sprintf "Unknown action %s" action)

(** [to_string field json] extracts string date from [field] in a
    [Yojson.Basic.t] object [json]. Calls [Utils.err_invalid_type] if type is
    incorrect*)
let to_string field json =
  match Basic.Util.member field json with
  | `String s -> s
  | (`List _ | `Assoc _) as t -> Utils.err_invalid_type true t
  | t -> Utils.err_invalid_type false t

(** [to_string_option field json] extracts string list data from [field] in a
    [Yojson.Basic.t] object [json]. Calls [Utils.err_invalid_type] if type is
    incorrect *)
let to_string_list field json =
  match Basic.Util.member field json with
  | `List l ->
    List.map
      (fun s ->
        match Basic.Util.to_string_option s with
        | None -> Utils.err_invalid_type false `Null
        | Some s -> s )
      l
  | `Assoc _a as t -> Utils.err_invalid_type true t
  | t -> Utils.err_invalid_type false t

(** [to_assoc_knv_list field json] extracts a [(string * Yojson.Basic.t) list]
    from [field] that is contained in the [json] object. Calls
    [Utils.err_invalid_type] if type is incorrect *)
let to_assoc_knv_list field json =
  match Basic.Util.member field json with
  | `Assoc _a as t -> Yojson.Basic.Util.to_assoc t
  | `List _l as t -> Utils.err_invalid_type true t
  | t -> Utils.err_invalid_type false t

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
  | `Assoc _a as t -> Utils.err_invalid_type true t
  | t -> Utils.err_invalid_type false t

(** [to_transition_table alphabet states transitions] returns a
    [ (state:string * read:string) (to_state:string * write:read * action:direction) Hashtbl.t].
    This function also runs several kinds of sanity checks on the validity of
    transitions described in the json file *)
let to_transition_table alphabet states (transitions : (string * Basic.t) list)
    =
  let transitbl = Hashtbl.create 512 in
  List.iter
    (fun (k, v) ->
      List.iter
        (fun (r, t) ->
          if not (List.mem r alphabet) then
            Utils.error
              (Format.sprintf {|"read" value %s is undefined in %s|} r k)
          else
            let collision = Hashtbl.find_opt transitbl (k, r) in
            match collision with
            | None -> (
              let to_state, write, _ = t in
              match (List.mem write alphabet, List.mem to_state states) with
              | false, false ->
                Utils.error
                  (Format.sprintf
                     {|"to_state" and "write" values (%s, %s) are undefined in transition %s|}
                     to_state write k )
              | false, true ->
                Utils.error
                  (Format.sprintf
                     {|"to_state" value %s is undefined in transition %s|}
                     to_state k )
              | true, false ->
                Utils.error
                  (Format.sprintf
                     {|"write" value %s is undefined in transition %s|} write k )
              | true, true -> Hashtbl.add transitbl (k, r) t )
            | Some _transitionstate ->
              Utils.error
                (Format.sprintf
                   {|Duplicate transition. State transition (%s) is already indexed somewhere in "transitions"@.|}
                   k ) )
        (get_transitions v) )
    transitions;
  transitbl

(** [to_transitions_tbl alphabet states field json] converts the value contained
    in [field] in the json object to [(string * Yojson.Basic.t) list] and feeds
    to the function handling the conversion to a type [Hashtbl.t] *)
let to_transitions_tbl alphabet states field = function
  | `Assoc _l as t ->
    to_assoc_knv_list field t |> to_transition_table alphabet states
  | `List _l as t -> Utils.err_invalid_type true t
  | t -> Utils.err_invalid_type false t

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
        Utils.error (Format.sprintf "State %s seems to be defined twice" state)
      )
    states;
  tbl

(** [get_value_and_check errmsg v check] simple generic function that takes an
    error message to feed to [Utils.error] if needed, a value [v] and a [check]
    function of type [(v:string -> bool)] *)
let get_value_and_check errmsg v check =
  if check v then
    v
  else
    Utils.error (Format.sprintf "%s %s" errmsg v)

(** [to_machnie jsonfile] function parses the jsonfile and does all necessary
    sanity checks and conversions for the interpreter *)
let to_machine jsonfile =
  let json =
    match Basic.from_file jsonfile with
    | exception Basic.Finally (_e1, _e2) ->
      Utils.error (Format.sprintf "Impossible to read file")
    | t -> t
  in
  let name =
    get_value_and_check "\"name\" field and filename are different:"
      (to_string "name" json)
      (String.equal (Filename.chop_extension jsonfile))
  in
  let alphabet =
    let alphabet_field_value = to_string_list "alphabet" json in
    if List.for_all (fun s -> String.length s = 1) alphabet_field_value then
      alphabet_field_value
    else
      Utils.error "Alphabet must be a list of strings of length equal to 1."
  in
  let blank =
    get_value_and_check "Alphabet does not contain blank:"
      (to_string "blank" json) (fun v -> List.mem v alphabet)
  in
  let states = to_string_list "states" json in
  let initial =
    get_value_and_check "States list does not contain initial state:"
      (to_string "initial" json) (fun v -> List.mem v states)
  in
  let finals =
    let finals_field_value = to_string_list "finals" json in
    List.iter
      (fun final ->
        if List.mem final states then
          ()
        else
          Utils.error final )
      finals_field_value;
    finals_field_value
  in
  let states_tbl = to_states_tbl initial finals states in
  let transitions_tbl =
    let transition_fields_value_tbl =
      to_transitions_tbl alphabet states "transitions" json
    in
    transition_fields_value_tbl
  in
  (name, alphabet, blank, states_tbl, initial, finals, transitions_tbl)
