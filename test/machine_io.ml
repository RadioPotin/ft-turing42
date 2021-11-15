let unary_sub_valid_input =
  [ ( "1-1="
    , {|[<1>-1=....] (scanright, 1) -> ( scanright, 1, RIGHT )
[1<->1=....] (scanright, -) -> ( scanright, -, RIGHT )
[1-<1>=....] (scanright, 1) -> ( scanright, 1, RIGHT )
[1-1<=>....] (scanright, =) -> ( eraseone, ., LEFT )
[1-<1>.....] (eraseone, 1) -> ( subone, =, LEFT )
[1<->=.....] (subone, -) -> ( skip, -, LEFT )
[<1>-=.....] (skip, 1) -> ( scanright, ., RIGHT )
[.<->=.....] (scanright, -) -> ( scanright, -, RIGHT )
[.-<=>.....] (scanright, =) -> ( eraseone, ., LEFT )
[.<->......] (eraseone, -) -> ( HALT, ., LEFT )
|}
    )
  ; ( "11-1="
    , {|[<1>1-1=.....] (scanright, 1) -> ( scanright, 1, RIGHT )
[1<1>-1=.....] (scanright, 1) -> ( scanright, 1, RIGHT )
[11<->1=.....] (scanright, -) -> ( scanright, -, RIGHT )
[11-<1>=.....] (scanright, 1) -> ( scanright, 1, RIGHT )
[11-1<=>.....] (scanright, =) -> ( eraseone, ., LEFT )
[11-<1>......] (eraseone, 1) -> ( subone, =, LEFT )
[11<->=......] (subone, -) -> ( skip, -, LEFT )
[1<1>-=......] (skip, 1) -> ( scanright, ., RIGHT )
[1.<->=......] (scanright, -) -> ( scanright, -, RIGHT )
[1.-<=>......] (scanright, =) -> ( eraseone, ., LEFT )
[1.<->.......] (eraseone, -) -> ( HALT, ., LEFT )
|}
    )
  ; ( "1-="
    , {|[<1>-=...] (scanright, 1) -> ( scanright, 1, RIGHT )
[1<->=...] (scanright, -) -> ( scanright, -, RIGHT )
[1-<=>...] (scanright, =) -> ( eraseone, ., LEFT )
[1<->....] (eraseone, -) -> ( HALT, ., LEFT )
|}
    )
  ]

let unary_sub_invalid_input =
  [ ( "\\"
    , {|[<\>.] (scanright, \) -> BLOCKED
transition (scanright, \) is undefined
|}
    )
  ; ( "="
    , {|[<=>..] (scanright, =) -> ( eraseone, ., LEFT )
[<.>...] (eraseone, .) -> BLOCKED
transition (eraseone, .) is undefined
|}
    )
  ]

let unary_sub_definition =
  {|********************************************************************************

*                                                                              *

*                             test_unary_sub_VALID                             *

*                                                                              *

********************************************************************************
Alphabet: [ 1, ., -, = ]
States  : [ scanright, eraseone, subone, HALT, skip ]
Initial : scanright
Finals  : [ HALT ]
(subone, -) -> ( skip, -, LEFT )
(subone, 1) -> ( subone, 1, LEFT )
(skip, .) -> ( skip, ., LEFT )
(skip, 1) -> ( scanright, ., RIGHT )
(scanright, =) -> ( eraseone, ., LEFT )
(scanright, .) -> ( scanright, ., RIGHT )
(scanright, 1) -> ( scanright, 1, RIGHT )
(scanright, -) -> ( scanright, -, RIGHT )
(eraseone, 1) -> ( subone, =, LEFT )
(eraseone, -) -> ( HALT, ., LEFT )
********************************************************************************
|}

let unary_add_valid_input =
  [ ( "1+1"
    , {|[<1>+1...] (skip, 1) -> ( skip, 1, RIGHT )
[1<+>1...] (skip, +) -> ( swap_init, ., RIGHT )
[1.<1>...] (swap_init, 1) -> ( swap_left, 1, LEFT )
[1<.>1...] (swap_left, .) -> ( swap_right, 1, RIGHT )
[11<1>...] (swap_right, 1) -> ( swap_init, ., RIGHT )
[11.<.>..] (swap_init, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ( "1+11"
    , {|[<1>+11....] (skip, 1) -> ( skip, 1, RIGHT )
[1<+>11....] (skip, +) -> ( swap_init, ., RIGHT )
[1.<1>1....] (swap_init, 1) -> ( swap_left, 1, LEFT )
[1<.>11....] (swap_left, .) -> ( swap_right, 1, RIGHT )
[11<1>1....] (swap_right, 1) -> ( swap_init, ., RIGHT )
[11.<1>....] (swap_init, 1) -> ( swap_left, 1, LEFT )
[11<.>1....] (swap_left, .) -> ( swap_right, 1, RIGHT )
[111<1>....] (swap_right, 1) -> ( swap_init, ., RIGHT )
[111.<.>...] (swap_init, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ( "11+1"
    , {|[<1>1+1....] (skip, 1) -> ( skip, 1, RIGHT )
[1<1>+1....] (skip, 1) -> ( skip, 1, RIGHT )
[11<+>1....] (skip, +) -> ( swap_init, ., RIGHT )
[11.<1>....] (swap_init, 1) -> ( swap_left, 1, LEFT )
[11<.>1....] (swap_left, .) -> ( swap_right, 1, RIGHT )
[111<1>....] (swap_right, 1) -> ( swap_init, ., RIGHT )
[111.<.>...] (swap_init, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ( "1+"
    , {|[<1>+..] (skip, 1) -> ( skip, 1, RIGHT )
[1<+>..] (skip, +) -> ( swap_init, ., RIGHT )
[1.<.>.] (swap_init, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ( "+1"
    , {|[<+>1..] (skip, +) -> ( swap_init, ., RIGHT )
[.<1>..] (swap_init, 1) -> ( swap_left, 1, LEFT )
[<.>1..] (swap_left, .) -> ( swap_right, 1, RIGHT )
[1<1>..] (swap_right, 1) -> ( swap_init, ., RIGHT )
[1.<.>.] (swap_init, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ( "1"
    , {|[<1>..] (skip, 1) -> ( skip, 1, RIGHT )
[1<.>.] (skip, .) -> ( HALT, ., RIGHT )
|}
    )
  ]

let unary_add_invalid_input =
  [ ( "+"
    , {|[<+>..] (skip, +) -> ( swap_init, ., RIGHT )
[.<.>.] (swap_init, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ("-", {|[<->.] (skip, -) -> BLOCKED
transition (skip, -) is undefined
|})
  ]

let unary_add_definition =
  {|********************************************************************************

*                                                                              *

*                                  unary_add                                   *

*                                                                              *

********************************************************************************
Alphabet: [ 1, ., + ]
States  : [ swap_right, swap_left, swap_init, HALT, skip ]
Initial : skip
Finals  : [ HALT ]
(swap_right, 1) -> ( swap_init, ., RIGHT )
(swap_left, .) -> ( swap_right, 1, RIGHT )
(swap_init, 1) -> ( swap_left, 1, LEFT )
(swap_init, .) -> ( HALT, ., RIGHT )
(skip, +) -> ( swap_init, ., RIGHT )
(skip, .) -> ( HALT, ., RIGHT )
(skip, 1) -> ( skip, 1, RIGHT )
********************************************************************************
|}

let zero_two_n_valid_input =
  [ ( "0"
    , {|[<0>....] (q1, 0) -> ( q2, ., RIGHT )
[.<.>...] (q2, .) -> ( TRUE, ., RIGHT )
[..<.>..] (TRUE, .) -> ( END, y, RIGHT )
[..y<.>.] (END, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ( "00"
    , {|[<0>0....] (q1, 0) -> ( q2, ., RIGHT )
[.<0>....] (q2, 0) -> ( q3, x, RIGHT )
[.x<.>...] (q3, .) -> ( q5, ., LEFT )
[.<x>....] (q5, x) -> ( q5, x, LEFT )
[<.>x....] (q5, .) -> ( q2, ., RIGHT )
[.<x>....] (q2, x) -> ( q2, x, RIGHT )
[.x<.>...] (q2, .) -> ( TRUE, ., RIGHT )
[.x.<.>..] (TRUE, .) -> ( END, y, RIGHT )
[.x.y<.>.] (END, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ( "000"
    , {|[<0>00....] (q1, 0) -> ( q2, ., RIGHT )
[.<0>0....] (q2, 0) -> ( q3, x, RIGHT )
[.x<0>....] (q3, 0) -> ( q4, 0, RIGHT )
[.x0<.>...] (q4, .) -> ( FALSE, ., RIGHT )
[.x0.<.>..] (FALSE, .) -> ( END, n, RIGHT )
[.x0.n<.>.] (END, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ( "0000"
    , {|[<0>000....] (q1, 0) -> ( q2, ., RIGHT )
[.<0>00....] (q2, 0) -> ( q3, x, RIGHT )
[.x<0>0....] (q3, 0) -> ( q4, 0, RIGHT )
[.x0<0>....] (q4, 0) -> ( q3, x, RIGHT )
[.x0x<.>...] (q3, .) -> ( q5, ., LEFT )
[.x0<x>....] (q5, x) -> ( q5, x, LEFT )
[.x<0>x....] (q5, 0) -> ( q5, 0, LEFT )
[.<x>0x....] (q5, x) -> ( q5, x, LEFT )
[<.>x0x....] (q5, .) -> ( q2, ., RIGHT )
[.<x>0x....] (q2, x) -> ( q2, x, RIGHT )
[.x<0>x....] (q2, 0) -> ( q3, x, RIGHT )
[.xx<x>....] (q3, x) -> ( q3, x, RIGHT )
[.xxx<.>...] (q3, .) -> ( q5, ., LEFT )
[.xx<x>....] (q5, x) -> ( q5, x, LEFT )
[.x<x>x....] (q5, x) -> ( q5, x, LEFT )
[.<x>xx....] (q5, x) -> ( q5, x, LEFT )
[<.>xxx....] (q5, .) -> ( q2, ., RIGHT )
[.<x>xx....] (q2, x) -> ( q2, x, RIGHT )
[.x<x>x....] (q2, x) -> ( q2, x, RIGHT )
[.xx<x>....] (q2, x) -> ( q2, x, RIGHT )
[.xxx<.>...] (q2, .) -> ( TRUE, ., RIGHT )
[.xxx.<.>..] (TRUE, .) -> ( END, y, RIGHT )
[.xxx.y<.>.] (END, .) -> ( HALT, ., RIGHT )
|}
    )
  ]

let zero_two_n_invalid_input =
  [ ( "01"
    , {|[<0>1..] (q1, 0) -> ( q2, ., RIGHT )
[.<1>..] (q2, 1) -> BLOCKED
transition (q2, 1) is undefined
|}
    )
  ]

let zero_two_n =
  {|********************************************************************************

*                                                                              *

*                                     02n                                      *

*                                                                              *

********************************************************************************
Alphabet: [ 0, ., x, y, n ]
States  : [ FALSE, TRUE, HALT, END, q5, q2, q3, q4, q1 ]
Initial : q1
Finals  : [ HALT ]
(q5, x) -> ( q5, x, LEFT )
(q5, 0) -> ( q5, 0, LEFT )
(q5, .) -> ( q2, ., RIGHT )
(q4, 0) -> ( q3, x, RIGHT )
(q4, x) -> ( q4, x, RIGHT )
(q4, .) -> ( FALSE, ., RIGHT )
(q3, x) -> ( q3, x, RIGHT )
(q3, .) -> ( q5, ., LEFT )
(q3, 0) -> ( q4, 0, RIGHT )
(q2, x) -> ( q2, x, RIGHT )
(q2, .) -> ( TRUE, ., RIGHT )
(q2, 0) -> ( q3, x, RIGHT )
(q1, 0) -> ( q2, ., RIGHT )
(q1, .) -> ( FALSE, ., RIGHT )
(q1, x) -> ( FALSE, x, RIGHT )
(TRUE, x) -> ( TRUE, x, RIGHT )
(TRUE, .) -> ( END, y, RIGHT )
(FALSE, x) -> ( FALSE, x, RIGHT )
(FALSE, .) -> ( END, n, RIGHT )
(END, .) -> ( HALT, ., RIGHT )
********************************************************************************
|}
