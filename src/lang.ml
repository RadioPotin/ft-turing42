open Yojson

exception Invalid_type of string
exception Duplicate_transition of string
exception Invalid_value of (string * Basic.t)
exception Not_found of string

type symbols = string list

type direction = Left | Right

type state = (string * bool * bool)

type transition =  (string * string * string * direction)

let from_direction = function
  | Right -> "RIGHT"
  | Left -> "LEFT"

let to_direction = function
  | "RIGHT" -> Right
  | "LEFT" -> Left
  | t -> raise (Invalid_type t)

let to_string field json =
    match Basic.Util.member field json with
    | `String s -> s
  | `Null -> raise (Not_found field)
  | _ -> raise (Invalid_type field)

let to_string_list field json =
    match Basic.Util.member field json with
  | `List  l ->
    List.map (fun s ->
      match Basic.Util.to_string_option s with
      | None -> raise (Invalid_type field)
      | Some s -> s
    ) l
  | `Null -> raise (Not_found field)
  | _ -> raise (Invalid_type field)

let to_transition_list = function
  | `Null as t-> raise (Invalid_value ("null", t))
  | `Bool b as t -> raise (Invalid_value (Format.sprintf "%b" b, t))
  | `Int i as t-> raise (Invalid_value (Format.sprintf "%i" i , t))
  | `Float f as t ->raise (Invalid_value (Format.sprintf "%f" f , t))
  | `String s as t -> raise (Invalid_value (Format.sprintf "%s" s, t))
  | `Assoc _a as t -> raise (Invalid_value ("assoc", t))
  | `List l ->
    List.map (fun l ->
      (to_string "read" l,
        to_string "to_state" l,
        to_string "write" l,
        to_direction (to_string "action" l))
    ) l

let transition_table transitions =
  let transitbl = Hashtbl.create 512 in
  List.iter (fun (k, v)->
    let collision = Hashtbl.find_opt transitbl k in
    match collision with
    | None -> Hashtbl.add transitbl k (to_transition_list v)
    | Some _x -> raise (Duplicate_transition k)) transitions;
  transitbl

let to_assoc_knv_list field json =
  match Basic.Util.member field json with
  | `List l as t ->
    raise (Invalid_value
        (Format.asprintf "%a"
            (Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
                (fun fmt t ->
                    Format.fprintf fmt "%s"
                      (Basic.pretty_to_string ~std:true t))) l
        , t))
  | `Null as t-> raise (Invalid_value ("null", t))
  | `Bool b as t -> raise (Invalid_value (Format.sprintf "%b" b, t))
  | `Int i as t-> raise (Invalid_value (Format.sprintf "%i" i , t))
  | `Float f as t ->raise (Invalid_value (Format.sprintf "%f" f , t))
  | `String s as t -> raise (Invalid_value (Format.sprintf "%s" s, t))
  | `Assoc _a as t-> Yojson.Basic.Util.to_assoc t

let to_transitions field = function
  | `List l -> raise (Invalid_value ("list", `List l))
  | `Null as t-> raise (Invalid_value ("null", t))
  | `Bool b as t -> raise (Invalid_value (Format.sprintf "%b" b, t))
  | `Int i as t-> raise (Invalid_value (Format.sprintf "%i" i , t))
  | `Float f as t ->raise (Invalid_value (Format.sprintf "%f" f , t))
  | `String s as t -> raise (Invalid_value (s, t))
  | `Assoc _l as t -> to_assoc_knv_list field t |> transition_table


let to_machine json =
  let name = to_string "name" json in
  let blank = to_string "blank" json in
  let initial = to_string "initial" json in
  let finals = to_string_list "finals" json in
  let states = to_string_list "states" json in
  let alphabet = to_string_list "alphabet" json in
  let transitions = to_transitions "transitions" json in
  (name, alphabet, blank, states, initial, finals, transitions)
