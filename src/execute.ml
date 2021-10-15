let convert input blank length =
  let tape = CCVector.make length blank in
  String.iteri (fun i cc -> CCVector.set tape i (String.make 1 cc)) input;
  tape

let tape_size = ref 0

let safe_set tape index write =
  if index < CCVector.size tape then
    CCVector.set tape index write
  else
    Utils.error "RESIZE"

let safe_read tape index =
  if index < CCVector.size tape then
    CCVector.get tape index
  else
    Utils.error "RESIZE"

let rec execution print
    ((_alphabet, _blank, state_tbl, transitions_tbl) as machine) tape index
    current_state =
  if index < 0 then
    Utils.error
      "Reached bottom end of tape, which is a problem i guess i dont know yet"
  else
    let read = safe_read tape index in
    match Hashtbl.find_opt transitions_tbl (current_state, read) with
    | None -> begin
      match Hashtbl.find_opt state_tbl current_state with
      | None ->
        Utils.error
          (Format.sprintf "NO TRANSITION for (%s, %s), AU PUIT@." current_state
             read )
      | Some roles -> (
        match roles with
        | false, false
        | true, false ->
          assert false
        | true, true
        | false, true ->
          () )
    end
    | Some ((next_state, write, direction) as transition) -> (
      let current_tape = CCVector.to_string ~sep:"" Fun.id tape in
      Pp.current_tape current_tape index
        ((current_state, read), transition)
        print;
      safe_set tape index write;
      match direction with
      | Right -> execution print machine tape (index + 1) next_state
      | Left -> execution print machine tape (index - 1) next_state )

let interpreter machine input =
  let alphabet, blank, initial, state_tbl, transitions_tbl = machine in
  let initial_length = String.length input in
  let tape1 = convert input blank (initial_length * 2) in
  let read = safe_read tape1 0 in
  if not (List.mem read alphabet) then
    Utils.error "AU PUIT"
  else (
    execution false
      (alphabet, blank, state_tbl, transitions_tbl)
      tape1 0 initial;
    tape_size := CCVector.size tape1;
    let tape2 = convert input blank !tape_size in
    execution true (alphabet, blank, state_tbl, transitions_tbl) tape2 0 initial
  )
