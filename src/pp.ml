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

let machine fmt
    (name, alphabet, _blank, states, initial_state, final_states, transitions) =
  Format.fprintf fmt
    {|%a
Alphabet: [ %a ]
States  : [ %a ]
Initial : %s
Finals  : [ %a ]
%a
%s|}
    header name
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       (fun fmt alphachar -> Format.fprintf fmt "%s" alphachar) )
    alphabet
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       (fun fmt state -> Format.fprintf fmt "%s" state) )
    (List.of_seq (Hashtbl.to_seq_keys states))
    initial_state
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       (fun fmt final -> Format.fprintf fmt "%s" final) )
    final_states
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
       (fun fmt ((state, read), (to_state, write, direction)) ->
         Format.fprintf fmt "(%s, %s) -> ( %s, %s, %s )" state read to_state
           write
           (Lang.from_direction direction) ) )
    (List.sort
       (fun ((s1, c1), _t1) ((s2, c2), _t2) ->
         match compare (String.length s2) (String.length s1) with
         | 0 -> compare c2 c1
         | n -> n )
       (List.of_seq (Hashtbl.to_seq transitions)) )
    divider
