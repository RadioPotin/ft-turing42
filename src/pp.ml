let divider =
  {|********************************************************************************|}

let header fmt name =
  let len = String.length name in
  Format.fprintf fmt
    {|%s

*                                                                              *

*%*s%s%*s*

*                                                                              *

%s|}
    divider
    ((78 - len) / 2)
    " " name
    (((78 - len) / 2) + 1)
    " " divider

let pp_transition fmt ((state, read), (to_state, write, direction)) =
  Format.fprintf fmt "(%s, %s) -> ( %s, %s, %s )" state read to_state write
    (Lang.from_direction direction)

let machine fmt
    (name, alphabet, _blank, states, initial_state, final_states, transitions) =
  Format.fprintf fmt
    {|%a
Alphabet: [ %a ]
States  : [ %a ]
Initial : %s
Finals  : [ %a ]
%a
%s@.|}
    header name
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       (fun fmt alphachar -> Format.fprintf fmt "%s" alphachar) )
    alphabet
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       (fun fmt state -> Format.fprintf fmt "%s" state) )
    (List.sort
       (fun s1 s2 -> compare (String.length s2) (String.length s1))
       (List.of_seq (Hashtbl.to_seq_keys states)) )
    initial_state
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       (fun fmt final -> Format.fprintf fmt "%s" final) )
    final_states
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
       pp_transition )
    (List.sort
       (fun ((s1, _c1), (to_state1, _, _)) ((s2, _c2), (to_state2, _, _)) ->
         match compare (String.length s2) (String.length s1) with
         | 0 -> compare (String.length to_state2) (String.length to_state1)
         | n -> n )
       (List.of_seq (Hashtbl.to_seq transitions)) )
    divider

let pp_tape fmt (tape, index) =
  String.iteri
    (fun i s ->
      if i = index then
        Format.fprintf fmt "<%c>" s
      else
        Format.fprintf fmt "%c" s )
    tape

let blocked_tape current_head state read print final_or_blocked =
  if print then
    Format.fprintf Format.std_formatter {|[%a] (%s, %s) -> %s@.|} pp_tape
      current_head state read final_or_blocked
  else
    Format.ifprintf Format.std_formatter {|[%a] (%s, %s) -> %s@.|} pp_tape
      current_head state read final_or_blocked

let current_tape tape index transition print =
  if print then
    Format.fprintf Format.std_formatter {|[%a] %a@.|} pp_tape (tape, index)
      pp_transition transition
  else
    Format.ifprintf Format.std_formatter {|[%a] %a@.|} pp_tape (tape, index)
      pp_transition transition
