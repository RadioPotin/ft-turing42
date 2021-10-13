let machine fmt
    (name, alphabet, blank, states, initial_state, final_states, transitions) =
  Format.fprintf fmt
    {|===NAME===@.%s@.===Alphabet===@.%a@.===BLANK===@.-- %s@.===STATES===@.%a@.===INITIAL===@.-- %s@.===FINALS===@.%a@.===TRANSITIONS===@.%a|}
    name
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
       (fun fmt alphachar -> Format.fprintf fmt "-- %s" alphachar) )
    alphabet blank
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
       (fun fmt state -> Format.fprintf fmt "-- %s" state) )
    states initial_state
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
       (fun fmt final -> Format.fprintf fmt "-- %s" final) )
    final_states
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
       (fun fmt ((state, read), (to_state, write, direction)) ->
         Format.fprintf fmt
           "STATE ---> (%s * %s): @.{@.to_state : %s@.write    : \
            %s@.direction: %s@.}@."
           state read to_state write
           (Lang.from_direction direction) ) )
    (List.of_seq (Hashtbl.to_seq transitions))
