let convert input blank length =
  let tape = CCVector.make length blank in
  String.iteri (fun i cc -> CCVector.set tape i (String.make 1 cc)) input;
  tape

let is_final tbl state =
  match Hashtbl.find_opt tbl state with
  | None -> false
  | Some answer -> (
    match answer with
    | _, false -> false
    | _, true -> true )

let move_direction =
  let open Lang in
  function
  | Right -> 1
  | Left -> -1

let is_blocked tape state read index state_tbl print =
  let is_blocked =
    Pp.blocked_tape (CCVector.to_string ~sep:"" Fun.id tape, index) state read
  in
  if is_final state_tbl state then
    is_blocked "BLOCKED IN FINAL STATE" print
  else
    is_blocked "BLOCKED" print

let tape_size = ref 0

let safe_set tape index write =
  if index < CCVector.size tape then
    CCVector.set tape index write
  else
    Utils.error "RESIZE SAFE SET"

let safe_read tape index =
  if index < CCVector.size tape then
    CCVector.get tape index
  else
    Utils.error "RESIZE SAFE READ"

let rec execution print ((state_tbl, transitions_tbl) as tables) tape index
    current_state =
  if index < 0 then
    Utils.error
      "Reached bottom end of tape, which is a problem i guess i dont know yet"
  else
    let read = safe_read tape index in
    match Hashtbl.find_opt transitions_tbl (current_state, read) with
    | None -> begin
      match Hashtbl.find_opt state_tbl current_state with
      | None -> is_blocked tape current_state read index state_tbl print
      | Some roles -> (
        match roles with
        | _, false -> is_blocked tape current_state read index state_tbl print
        | _, true -> () )
    end
    | Some ((next_state, write, direction) as transition) ->
      let current_tape = CCVector.to_string ~sep:"" Fun.id tape in
      Pp.current_tape current_tape index
        ((current_state, read), transition)
        print;
      safe_set tape index write;
      execution print tables tape (index + move_direction direction) next_state

let interpreter machine input =
  let alphabet, blank, initial, state_tbl, transitions_tbl = machine in
  let initial_length =
    match String.length input with
    | 0 -> 5
    | n -> n
  in
  let tape1 = convert input blank (initial_length * 2) in
  let read = safe_read tape1 0 in
  if not (List.mem read alphabet) then
    is_blocked tape1 initial read 0 state_tbl true
  else (
    execution false (state_tbl, transitions_tbl) tape1 0 initial;
    tape_size := CCVector.size tape1;
    let tape2 = convert input blank !tape_size in
    execution true (state_tbl, transitions_tbl) tape2 0 initial
  )
