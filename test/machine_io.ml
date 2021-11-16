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
  [ ( "="
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
  []

let zero_two_n_definition =
  {|********************************************************************************

*                                                                              *

*                                     02n                                      *

*                                                                              *

********************************************************************************
Alphabet: [ 0, . ]
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

let palindrome_valid_input =
  [ ( "0"
    , {|[<0>....] (q0, 0) -> ( q1, ., RIGHT )
[.<.>...] (q1, .) -> ( TRUE, ., RIGHT )
[..<.>..] (TRUE, .) -> ( END, y, RIGHT )
[..y<.>.] (END, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ( "1"
    , {|[<1>....] (q0, 1) -> ( q2, ., RIGHT )
[.<.>...] (q2, .) -> ( TRUE, ., RIGHT )
[..<.>..] (TRUE, .) -> ( END, y, RIGHT )
[..y<.>.] (END, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ( "00"
    , {|[<0>0...] (q0, 0) -> ( q1, ., RIGHT )
[.<0>...] (q1, 0) -> ( q3, 0, RIGHT )
[.0<.>..] (q3, .) -> ( q5, ., LEFT )
[.<0>...] (q5, 0) -> ( q7, ., LEFT )
[<.>....] (q7, .) -> ( q0, ., RIGHT )
[.<.>...] (q0, .) -> ( TRUE, ., RIGHT )
[..<.>..] (TRUE, .) -> ( END, y, RIGHT )
[..y<.>.] (END, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ( "11"
    , {|[<1>1...] (q0, 1) -> ( q2, ., RIGHT )
[.<1>...] (q2, 1) -> ( q4, 1, RIGHT )
[.1<.>..] (q4, .) -> ( q6, ., LEFT )
[.<1>...] (q6, 1) -> ( q7, ., LEFT )
[<.>....] (q7, .) -> ( q0, ., RIGHT )
[.<.>...] (q0, .) -> ( TRUE, ., RIGHT )
[..<.>..] (TRUE, .) -> ( END, y, RIGHT )
[..y<.>.] (END, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ( "101"
    , {|[<1>01...] (q0, 1) -> ( q2, ., RIGHT )
[.<0>1...] (q2, 0) -> ( q4, 0, RIGHT )
[.0<1>...] (q4, 1) -> ( q4, 1, RIGHT )
[.01<.>..] (q4, .) -> ( q6, ., LEFT )
[.0<1>...] (q6, 1) -> ( q7, ., LEFT )
[.<0>....] (q7, 0) -> ( q7, 0, LEFT )
[<.>0....] (q7, .) -> ( q0, ., RIGHT )
[.<0>....] (q0, 0) -> ( q1, ., RIGHT )
[..<.>...] (q1, .) -> ( TRUE, ., RIGHT )
[...<.>..] (TRUE, .) -> ( END, y, RIGHT )
[...y<.>.] (END, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ( "010"
    , {|[<0>10...] (q0, 0) -> ( q1, ., RIGHT )
[.<1>0...] (q1, 1) -> ( q3, 1, RIGHT )
[.1<0>...] (q3, 0) -> ( q3, 0, RIGHT )
[.10<.>..] (q3, .) -> ( q5, ., LEFT )
[.1<0>...] (q5, 0) -> ( q7, ., LEFT )
[.<1>....] (q7, 1) -> ( q7, 1, LEFT )
[<.>1....] (q7, .) -> ( q0, ., RIGHT )
[.<1>....] (q0, 1) -> ( q2, ., RIGHT )
[..<.>...] (q2, .) -> ( TRUE, ., RIGHT )
[...<.>..] (TRUE, .) -> ( END, y, RIGHT )
[...y<.>.] (END, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ( "0110"
    , {|[<0>110....] (q0, 0) -> ( q1, ., RIGHT )
[.<1>10....] (q1, 1) -> ( q3, 1, RIGHT )
[.1<1>0....] (q3, 1) -> ( q3, 1, RIGHT )
[.11<0>....] (q3, 0) -> ( q3, 0, RIGHT )
[.110<.>...] (q3, .) -> ( q5, ., LEFT )
[.11<0>....] (q5, 0) -> ( q7, ., LEFT )
[.1<1>.....] (q7, 1) -> ( q7, 1, LEFT )
[.<1>1.....] (q7, 1) -> ( q7, 1, LEFT )
[<.>11.....] (q7, .) -> ( q0, ., RIGHT )
[.<1>1.....] (q0, 1) -> ( q2, ., RIGHT )
[..<1>.....] (q2, 1) -> ( q4, 1, RIGHT )
[..1<.>....] (q4, .) -> ( q6, ., LEFT )
[..<1>.....] (q6, 1) -> ( q7, ., LEFT )
[.<.>......] (q7, .) -> ( q0, ., RIGHT )
[..<.>.....] (q0, .) -> ( TRUE, ., RIGHT )
[...<.>....] (TRUE, .) -> ( END, y, RIGHT )
[...y<.>...] (END, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ( "11011"
    , {|[<1>1011.....] (q0, 1) -> ( q2, ., RIGHT )
[.<1>011.....] (q2, 1) -> ( q4, 1, RIGHT )
[.1<0>11.....] (q4, 0) -> ( q4, 0, RIGHT )
[.10<1>1.....] (q4, 1) -> ( q4, 1, RIGHT )
[.101<1>.....] (q4, 1) -> ( q4, 1, RIGHT )
[.1011<.>....] (q4, .) -> ( q6, ., LEFT )
[.101<1>.....] (q6, 1) -> ( q7, ., LEFT )
[.10<1>......] (q7, 1) -> ( q7, 1, LEFT )
[.1<0>1......] (q7, 0) -> ( q7, 0, LEFT )
[.<1>01......] (q7, 1) -> ( q7, 1, LEFT )
[<.>101......] (q7, .) -> ( q0, ., RIGHT )
[.<1>01......] (q0, 1) -> ( q2, ., RIGHT )
[..<0>1......] (q2, 0) -> ( q4, 0, RIGHT )
[..0<1>......] (q4, 1) -> ( q4, 1, RIGHT )
[..01<.>.....] (q4, .) -> ( q6, ., LEFT )
[..0<1>......] (q6, 1) -> ( q7, ., LEFT )
[..<0>.......] (q7, 0) -> ( q7, 0, LEFT )
[.<.>0.......] (q7, .) -> ( q0, ., RIGHT )
[..<0>.......] (q0, 0) -> ( q1, ., RIGHT )
[...<.>......] (q1, .) -> ( TRUE, ., RIGHT )
[....<.>.....] (TRUE, .) -> ( END, y, RIGHT )
[....y<.>....] (END, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ( "10"
    , {|[<1>0...] (q0, 1) -> ( q2, ., RIGHT )
[.<0>...] (q2, 0) -> ( q4, 0, RIGHT )
[.0<.>..] (q4, .) -> ( q6, ., LEFT )
[.<0>...] (q6, 0) -> ( FALSE, 0, RIGHT )
[.0<.>..] (FALSE, .) -> ( END, n, RIGHT )
[.0n<.>.] (END, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ( "011"
    , {|[<0>11...] (q0, 0) -> ( q1, ., RIGHT )
[.<1>1...] (q1, 1) -> ( q3, 1, RIGHT )
[.1<1>...] (q3, 1) -> ( q3, 1, RIGHT )
[.11<.>..] (q3, .) -> ( q5, ., LEFT )
[.1<1>...] (q5, 1) -> ( FALSE, n, LEFT )
[.<1>n...] (FALSE, 1) -> ( FALSE, 1, RIGHT )
[.1<n>...] (FALSE, n) -> ( END, n, RIGHT )
[.1n<.>..] (END, .) -> ( HALT, ., RIGHT )
|}
    )
  ; ( "0101"
    , {|[<0>101....] (q0, 0) -> ( q1, ., RIGHT )
[.<1>01....] (q1, 1) -> ( q3, 1, RIGHT )
[.1<0>1....] (q3, 0) -> ( q3, 0, RIGHT )
[.10<1>....] (q3, 1) -> ( q3, 1, RIGHT )
[.101<.>...] (q3, .) -> ( q5, ., LEFT )
[.10<1>....] (q5, 1) -> ( FALSE, n, LEFT )
[.1<0>n....] (FALSE, 0) -> ( FALSE, 0, RIGHT )
[.10<n>....] (FALSE, n) -> ( END, n, RIGHT )
[.10n<.>...] (END, .) -> ( HALT, ., RIGHT )
|}
    )
  ]

let palindrome_invalid_input =
  [   ]

let palindrome_definition =
  {|********************************************************************************

*                                                                              *

*                                  palindrome                                  *

*                                                                              *

********************************************************************************
Alphabet: [ 0, 1, . ]
States  : [ FALSE, TRUE, HALT, END, q5, q2, q0, q3, q6, q4, q1, q7 ]
Initial : q0
Finals  : [ HALT ]
(q7, 1) -> ( q7, 1, LEFT )
(q7, 0) -> ( q7, 0, LEFT )
(q7, .) -> ( q0, ., RIGHT )
(q6, 0) -> ( FALSE, 0, RIGHT )
(q6, 1) -> ( q7, ., LEFT )
(q5, 1) -> ( FALSE, n, LEFT )
(q5, 0) -> ( q7, ., LEFT )
(q4, 0) -> ( q4, 0, RIGHT )
(q4, .) -> ( q6, ., LEFT )
(q4, 1) -> ( q4, 1, RIGHT )
(q3, .) -> ( q5, ., LEFT )
(q3, 0) -> ( q3, 0, RIGHT )
(q3, 1) -> ( q3, 1, RIGHT )
(q2, 1) -> ( q4, 1, RIGHT )
(q2, .) -> ( TRUE, ., RIGHT )
(q2, 0) -> ( q4, 0, RIGHT )
(q1, 0) -> ( q3, 0, RIGHT )
(q1, .) -> ( TRUE, ., RIGHT )
(q1, 1) -> ( q3, 1, RIGHT )
(q0, 0) -> ( q1, ., RIGHT )
(q0, .) -> ( TRUE, ., RIGHT )
(q0, 1) -> ( q2, ., RIGHT )
(TRUE, 0) -> ( FALSE, 0, RIGHT )
(TRUE, 1) -> ( FALSE, 1, RIGHT )
(TRUE, y) -> ( END, y, RIGHT )
(TRUE, .) -> ( END, y, RIGHT )
(FALSE, n) -> ( END, n, RIGHT )
(FALSE, 0) -> ( FALSE, 0, RIGHT )
(FALSE, .) -> ( END, n, RIGHT )
(FALSE, 1) -> ( FALSE, 1, RIGHT )
(END, .) -> ( HALT, ., RIGHT )
(END, 0) -> ( HALT, 0, RIGHT )
(END, 1) -> ( HALT, 1, RIGHT )
********************************************************************************
|}

let zero_n_one_n_valid_input =
  [ ( "0"
    , {|[<0>..] (q0, 0) -> ( q1, a, RIGHT )
[a<.>.] (q1, .) -> ( FALSE, ., LEFT )
[<a>..] (FALSE, a) -> ( FALSE, a, RIGHT )
[a<.>.] (FALSE, .) -> ( END, n, RIGHT )
[an<.>] (END, .) -> ( HALT, ., LEFT )
|}
    )
  ; ( "1"
    , {|[<1>..] (q0, 1) -> ( FALSE, 1, RIGHT )
[1<.>.] (FALSE, .) -> ( END, n, RIGHT )
[1n<.>] (END, .) -> ( HALT, ., LEFT )
|}
    )
  ; ( "01"
    , {|[<0>1..] (q0, 0) -> ( q1, a, RIGHT )
[a<1>..] (q1, 1) -> ( q2, b, LEFT )
[<a>b..] (q2, a) -> ( q0, a, RIGHT )
[a<b>..] (q0, b) -> ( q3, b, RIGHT )
[ab<.>.] (q3, .) -> ( TRUE, ., LEFT )
[a<b>..] (TRUE, b) -> ( TRUE, b, RIGHT )
[ab<.>.] (TRUE, .) -> ( END, y, RIGHT )
[aby<.>] (END, .) -> ( HALT, ., LEFT )
|}
    )
  ; ( "011"
    , {|[<0>11...] (q0, 0) -> ( q1, a, RIGHT )
[a<1>1...] (q1, 1) -> ( q2, b, LEFT )
[<a>b1...] (q2, a) -> ( q0, a, RIGHT )
[a<b>1...] (q0, b) -> ( q3, b, RIGHT )
[ab<1>...] (q3, 1) -> ( FALSE, 1, LEFT )
[a<b>1...] (FALSE, b) -> ( FALSE, b, RIGHT )
[ab<1>...] (FALSE, 1) -> ( FALSE, 1, RIGHT )
[ab1<.>..] (FALSE, .) -> ( END, n, RIGHT )
[ab1n<.>.] (END, .) -> ( HALT, ., LEFT )
|}
    )
  ]

let zero_n_one_n_invalid_input = [  ]

let zero_n_one_n_definition =
  {|********************************************************************************

*                                                                              *

*                                     0n1n                                     *

*                                                                              *

********************************************************************************
Alphabet: [ 0, 1, . ]
States  : [ FALSE, TRUE, HALT, END, q2, q0, q3, q4, q1 ]
Initial : q0
Finals  : [ HALT ]
(q3, .) -> ( TRUE, ., LEFT )
(q3, 0) -> ( FALSE, 0, LEFT )
(q3, b) -> ( q3, b, RIGHT )
(q3, 1) -> ( FALSE, 1, LEFT )
(q2, 0) -> ( q2, 0, LEFT )
(q2, a) -> ( q0, a, RIGHT )
(q2, b) -> ( q2, b, LEFT )
(q1, 0) -> ( q1, 0, RIGHT )
(q1, .) -> ( FALSE, ., LEFT )
(q1, 1) -> ( q2, b, LEFT )
(q1, b) -> ( q1, b, RIGHT )
(q0, 0) -> ( q1, a, RIGHT )
(q0, b) -> ( q3, b, RIGHT )
(q0, 1) -> ( FALSE, 1, RIGHT )
(TRUE, 0) -> ( TRUE, 0, RIGHT )
(TRUE, a) -> ( TRUE, a, RIGHT )
(TRUE, b) -> ( TRUE, b, RIGHT )
(TRUE, 1) -> ( TRUE, 1, RIGHT )
(TRUE, .) -> ( END, y, RIGHT )
(FALSE, b) -> ( FALSE, b, RIGHT )
(FALSE, 0) -> ( FALSE, 0, RIGHT )
(FALSE, .) -> ( END, n, RIGHT )
(FALSE, a) -> ( FALSE, a, RIGHT )
(FALSE, 1) -> ( FALSE, 1, RIGHT )
(END, .) -> ( HALT, ., LEFT )
********************************************************************************
|}
