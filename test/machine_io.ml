let unary_sub_valid_input =
  [ ( "1-1="
    , "[<1>-1=....] (scanright, 1) -> ( scanright, 1, RIGHT )\n\
       [1<->1=....] (scanright, -) -> ( scanright, -, RIGHT )\n\
       [1-<1>=....] (scanright, 1) -> ( scanright, 1, RIGHT )\n\
       [1-1<=>....] (scanright, =) -> ( eraseone, ., LEFT )\n\
       [1-<1>.....] (eraseone, 1) -> ( subone, =, LEFT )\n\
       [1<->=.....] (subone, -) -> ( skip, -, LEFT )\n\
       [<1>-=.....] (skip, 1) -> ( scanright, ., RIGHT )\n\
       [.<->=.....] (scanright, -) -> ( scanright, -, RIGHT )\n\
       [.-<=>.....] (scanright, =) -> ( eraseone, ., LEFT )\n\
       [.<->......] (eraseone, -) -> ( HALT, ., LEFT )\n" )
  ; ( "11-1="
    , "[<1>1-1=.....] (scanright, 1) -> ( scanright, 1, RIGHT )\n\
       [1<1>-1=.....] (scanright, 1) -> ( scanright, 1, RIGHT )\n\
       [11<->1=.....] (scanright, -) -> ( scanright, -, RIGHT )\n\
       [11-<1>=.....] (scanright, 1) -> ( scanright, 1, RIGHT )\n\
       [11-1<=>.....] (scanright, =) -> ( eraseone, ., LEFT )\n\
       [11-<1>......] (eraseone, 1) -> ( subone, =, LEFT )\n\
       [11<->=......] (subone, -) -> ( skip, -, LEFT )\n\
       [1<1>-=......] (skip, 1) -> ( scanright, ., RIGHT )\n\
       [1.<->=......] (scanright, -) -> ( scanright, -, RIGHT )\n\
       [1.-<=>......] (scanright, =) -> ( eraseone, ., LEFT )\n\
       [1.<->.......] (eraseone, -) -> ( HALT, ., LEFT )\n" )
  ; ( "1-="
    , "[<1>-=...] (scanright, 1) -> ( scanright, 1, RIGHT )\n\
       [1<->=...] (scanright, -) -> ( scanright, -, RIGHT )\n\
       [1-<=>...] (scanright, =) -> ( eraseone, ., LEFT )\n\
       [1<->....] (eraseone, -) -> ( HALT, ., LEFT )\n" )
  ]

let unary_sub_invalid_input =
  [ ( "\\"
    , "[<\\>.] (scanright, \\) -> BLOCKED\n\
       transition (scanright, \\) is undefined\n" )
  ; ( "="
    , "[<=>..] (scanright, =) -> ( eraseone, ., LEFT )\n\
       [<.>...] (eraseone, .) -> BLOCKED\n\
       transition (eraseone, .) is undefined\n" )
  ]

let unary_add_valid_input =
  [
    ("1+1",
      {|[<1>+1...] (skip, 1) -> ( skip, 1, RIGHT )
[1<+>1...] (skip, +) -> ( swap_init, ., RIGHT )
[1.<1>...] (swap_init, 1) -> ( swap_left, 1, LEFT )
[1<.>1...] (swap_left, .) -> ( swap_right, 1, RIGHT )
[11<1>...] (swap_right, 1) -> ( swap_init, ., RIGHT )
[11.<.>..] (swap_init, .) -> ( HALT, ., RIGHT )
|})
  ;("1+11",
    {|[<1>+11....] (skip, 1) -> ( skip, 1, RIGHT )
[1<+>11....] (skip, +) -> ( swap_init, ., RIGHT )
[1.<1>1....] (swap_init, 1) -> ( swap_left, 1, LEFT )
[1<.>11....] (swap_left, .) -> ( swap_right, 1, RIGHT )
[11<1>1....] (swap_right, 1) -> ( swap_init, ., RIGHT )
[11.<1>....] (swap_init, 1) -> ( swap_left, 1, LEFT )
[11<.>1....] (swap_left, .) -> ( swap_right, 1, RIGHT )
[111<1>....] (swap_right, 1) -> ( swap_init, ., RIGHT )
[111.<.>...] (swap_init, .) -> ( HALT, ., RIGHT )
|})
  ]

let unary_add_invalid_input =
  [
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
