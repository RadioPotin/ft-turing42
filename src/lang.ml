open Yojson

exception Invalid_type of string

exception Not_found of string

type symbols = string list

type direction =
  | Left
  | Right

type state = string * bool * bool

type transition = string * string * direction

let from_direction = function
  | Right -> "RIGHT"
  | Left -> "LEFT"

let to_direction = function
  | "RIGHT" -> Right
  | "LEFT" -> Left
  | action -> Utils.error (Format.sprintf "Unknown action %s" action)

let to_string field json =
  match Basic.Util.member field json with
  | `String s -> s
  | (`List _ | `Assoc _) as t -> Utils.err_invalid_type true t
  | t -> Utils.err_invalid_type false t

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

let to_assoc_knv_list field json =
  match Basic.Util.member field json with
  | `List _l as t -> Utils.err_invalid_type true t
  | `Null as t -> Utils.err_invalid_type false t
  | `Bool _b as t -> Utils.err_invalid_type false t
  | `Int _i as t -> Utils.err_invalid_type false t
  | `Float _f as t -> Utils.err_invalid_type false t
  | `String _s as t -> Utils.err_invalid_type false t
  | `Assoc _a as t -> Yojson.Basic.Util.to_assoc t

let get_transitions = function
  | `Assoc _a as t -> Utils.err_invalid_type true t
  | `Null as t -> Utils.err_invalid_type false t
  | `Bool _b as t -> Utils.err_invalid_type false t
  | `Int _i as t -> Utils.err_invalid_type false t
  | `Float _f as t -> Utils.err_invalid_type false t
  | `String _s as t -> Utils.err_invalid_type false t
  | `List l ->
    List.map
      (fun l ->
        ( to_string "read" l
        , ( to_string "to_state" l
          , to_string "write" l
          , to_direction (to_string "action" l) ) ) )
      l

let to_transition_table alphabet states (transitions : (string * Basic.t) list)
    =
  let transitbl = Hashtbl.create 512 in
  List.iter
    (fun (k, v) ->
      List.iter
        (fun (r, t) ->
          if not (List.exists (String.equal r) alphabet) then
            Utils.error
              (Format.sprintf {|"read" value %s is undefined in %s|} r k)
          else
            let collision = Hashtbl.find_opt transitbl (k, r) in
            match collision with
            | None -> (
              let to_state, write, _ = t in
              match
                ( List.exists (String.equal write) alphabet
                , List.exists (String.equal to_state) states )
              with
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

let to_transitions alphabet states field = function
  | `List _l as t -> Utils.err_invalid_type true t
  | `Null as t -> Utils.err_invalid_type false t
  | `Bool _b as t -> Utils.err_invalid_type false t
  | `Int _i as t -> Utils.err_invalid_type false t
  | `Float _f as t -> Utils.err_invalid_type false t
  | `String _s as t -> Utils.err_invalid_type false t
  | `Assoc _l as t ->
    to_assoc_knv_list field t |> to_transition_table alphabet states

let get_value_and_check err v check =
  if check v then
    v
  else
    Utils.error (Format.sprintf "%s %s" err v)

let to_machine jsonfile =
  let json = Basic.from_file jsonfile in
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
      (to_string "blank" json) (fun v -> List.exists (String.equal v) alphabet)
  in
  let states = to_string_list "states" json in
  let initial =
    get_value_and_check "States list does not contain initial state:"
      (to_string "initial" json) (fun v -> List.exists (String.equal v) states)
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
  let transitions =
    let transition_fields_value_tbl =
      to_transitions alphabet states "transitions" json
    in

    transition_fields_value_tbl
  in
  (name, alphabet, blank, states, initial, finals, transitions)
