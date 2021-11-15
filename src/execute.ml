(** [check_input input blank] does sanity checks on input and returns its length
    if all is good *)
let check_input input blank =
  if String.contains input (String.get blank 0) then
    Utils.wrap_error Utils.error "Invalid input, blank character cannot be in input";
  match String.length input with
  | 0 -> Utils.wrap_error Utils.error "Invalid input, length = 0"
  | n -> n

(** [convert input blank length] convert a string input into a resizable vector *)
let convert input blank length =
  let tape = CCVector.make length blank in
  String.iteri (fun i cc -> CCVector.set tape i (String.make 1 cc)) input;
  tape

(** [is_final tbl state] simple boolean to check if a state is contained in the
    Hashtbl of all known states *)
let is_final tbl state =
  match Hashtbl.find_opt tbl state with
  | None -> false
  | Some answer -> (
    match answer with
    | _, false -> false
    | _, true -> true )

(** [is_blocked tape state read index state_tbl print] *)
let is_blocked fmt tape state read index ~print =
  let is_blocked =
    Pp.blocked_tape fmt
      (CCVector.to_string ~sep:"" Fun.id tape, index)
      state read print
  in
  is_blocked
    (Format.sprintf "BLOCKED@.transition (%s, %s) is undefined" state read)

(** [terminate current_state read print tape index state_tbl] is called when
    machine is estimated to be in a final or blocked state *)
let terminate fmt current_state read print tape index state_tbl =
  if is_final state_tbl current_state then
    ()
  else
    is_blocked fmt tape current_state read index ~print

(** [move_direction direction] converts a given action field direction to an
    incrementation or decrementation of the index in the recursive
    interpretation *)
let move_direction =
  let open Lang in
  function
  | Right -> 1
  | Left -> -1

let tape_size = ref 0

let blank_char = ref ""

let index_checker index tape =
  let size = CCVector.size tape in
  if index < 0 then (
    CCVector.rev_in_place tape;
    CCVector.push tape !blank_char;
    CCVector.rev_in_place tape;
    0
  ) else if index >= size then (
    CCVector.push tape !blank_char;
    index
  ) else
    index

let safe_set tape index write =
  if index < CCVector.size tape then
    CCVector.set tape index write
  else
    assert false

let safe_read tape index =
  if index < CCVector.size tape then
    CCVector.get tape index
  else
    assert false

(** [execution tables tape index current_state] recursively reads and writes on
    given [tape] at given [index] in regards to [current state] by following the
    transitions found in transitions and states [tables] *)
let rec execution fmt ~print ((state_tbl, transitions_tbl) as tables) tape index
    current_state =
  let index = index_checker index tape in
  let read = safe_read tape index in
  match Hashtbl.find_opt transitions_tbl (current_state, read) with
  | None -> terminate fmt current_state read print tape index state_tbl
  | Some ((next_state, write, direction) as transition) ->
    let current_tape = CCVector.to_string ~sep:"" Fun.id tape in
    Pp.current_tape fmt current_tape index
      ((current_state, read), transition)
      print;
    safe_set tape index write;
    execution fmt ~print tables tape
      (index + move_direction direction)
      next_state

(** [interpreter machine input] function sets up the execution of the turing
    machine [machine] on the [input]. *)
let interpreter fmt (machine, input) =
  let alphabet, blank, initial, tables = machine in
  let initial_length = check_input input blank in
  let tape1 = convert input blank (initial_length * 2) in
  let read = safe_read tape1 0 in
  if not (List.mem read alphabet) then
    is_blocked fmt tape1 initial read 0 ~print:true
  else (
    blank_char := blank;
    execution fmt ~print:false tables tape1 0 initial;
    tape_size := CCVector.size tape1;
    let tape2 = convert input blank !tape_size in
    execution fmt ~print:true tables tape2 0 initial
  )
