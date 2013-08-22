* factor an arbitrarily large positive integer
*
* Copyright (C) 1999 by Brian Raiter
* under the GNU General Public License
*
* Taken from http://www.muppetlabs.com/~breadbox/bf/factor.b.txt

>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>-

*
* read in the number
*

<<<<<<<<<+
[-[>>>>>>>>>>][-]<<<<<<<<<<[[->>>>>>>>>>+<<<<<<<<<<]<<<<<<<<<<]
  >>>>>>>>>>,----------]
>>>>>>>>>>[------------------------------------->>>>>>>>>->]
<[+>[>>>>>>>>>+>]<-<<<<<<<<<<]-

*
* display the number and initialize the loop variable to two
*

[>++++++++++++++++++++++++++++++++++++++++++++++++.
  ------------------------------------------------<<<<<<<<<<<]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.
--------------------------.[-]
>>>>>>>>>>>>++<<<<+

*
* the main loop
*

[ [-]>>

  *
  * make copies of the number and the loop variable
  *

  [>>>>[-]>[-]>[-]>[-]
    >[-]>[-]
    <<<<<<<[->>>+>+<<<<]>>>>>>>>]
  <<<<<<<<<<[>>>>>>[-<<<<+>>>>]<<<<<<<<<<<<<<<<]>>>>>>>>>>
  [>[->>>+>>+<<<<<]>>>>>>>>>]
  <<<<<<<<<<[>>>>>>[-<<<<<+>>>>>]<<<<<<<<<<<<<<<<]>>>>>>>>>>

  *
  * divide the number by the loop variable
  *

  [>>>[-]>>>[-]>[-]>>>]                                  initialize
  <<<<<<<<<<[<<<<<<<<<<]
  >>>>>>>>>[-]>>>>>>>+<<<<<<<<[+]+
  [ ->>                               double divisor until above dividend
    [>>>>>>[->++<]>>>>]<<<<<<<<<<
    [>>>>>>>>[-]>[-]
       <<<<[->>>++<<<]<<<<<<<<<<<<<<<]>>>>>>>>>>
    [>>>>>>>>[->+<[->+<[->+<[->+<[->+<[->+<[->+<[->+<[->+<
            [->--------->>>>>>>>>+<<<<<<<<<<[->+<]]]]]]]]]]]>>]
    <<<<<<<<<<[>>>>>>>>>[-<+<<<+>>>>]<<<<<<<<<<<<<<<<<<<]>>>>>>>>>>
    [>>>>>>>[-<+>[-<+>[-<+>[-<+>[-<+>[-<+>[-<+>[-<+>[-<+>
            [-<--------->>>>>>>>>>>+<<<<<<<<<<[-<+>]]]]]]]]]]]>>>]
    <<<<<<<<<<
    [>>>>[->>>+>>+<<<<<]<<<<<<<<<<<<<<]
    >>>>>>>>>>[>>>>>>>[-<<<+>>>]>>>]<<<<<<<<<<
    [>>>>>>>>[->-<]>
      [<<<<<<<<<[<[-]>>>>>>>>>>[-<<<<<<<<<<+>>>>>>>>>>]<<<<<<<<<<<<<<<<<<<]
        >>>>>>>>>>>>>>>>>>>]
      <<<<<<<<<<<<<<<<<<<]
    >>>>>>>>>[+[+[+[+[+[+[+[+[+[+[[-]<+>]]]]]]]]]]]<
  ]
  >>>>>>>>
  [                                   subtract divisor from dividend
    <<<<<<
    [>>>>>>>>[-]>[-]<<<<<[->>>+>+<<<<]>>>>>>]<<<<<<<<<<
    [>>>>>>>>[-<<<<+>>>>]<<<[->>>+>+<<<<]<<<<<<<<<<<<<<<]>>>>>>>>>>
    [>>>>>>>>>[-<<<<+>>>>]>]<<<<<<<<<<
    [>>>>>>>>[-<->]<<<<<<<<<<<<<<<<<<]>>>>>>>>>>
    [>>>>>>>[->+<[->+<[->+<[->+<[->+<[->+<[->+<[->+<[->+<[->+<
            [++++++++++[+>-<]>>>>>>>>>>-<<<<<<<<<<]]]]]]]]]]]>>>]
    >>>>>>>+
    [                                 if difference is nonnegative then
      [-]<<<<<<<<<<<<<<<<<            replace dividend and increment quotient
      [>>>>[-]>>>>[-<<<<+>>>>]<<[->>+<<]<<<<<<<<<<<<<<<<]>>>>>>>>>>
      [>>>>>>>>[->+<<<+>>]>>]<<<<<<<<<<
      [>>>[->>>>>>+<<<<<<]<<<<<<<<<<<<<]>>>>>>>>>>
      [>>>>>>>>>[-<<<<<<+>>>>>>[-<<<<<<+>>>>>>
                [-<<<<<<+>>>>>>[-<<<<<<+>>>>>>
                [-<<<<<<+>>>>>>[-<<<<<<+>>>>>>
                [-<<<<<<+>>>>>>[-<<<<<<+>>>>>>
                [-<<<<<<+>>>>>>[-<<<<<<--------->>>>>>>>>>>>>>>>+<<<<<<<<<<
                [-<<<<<<+>>>>>>]]]]]]]]]]]>]
      >>>>>>>
    ]                                 halve divisor and loop until zero
    <<<<<<<<<<<<<<<<<[<<<<<<<<<<]>>>>>>>>>>
    [>>>>>>>>[-]<<[->+<]<[->>>+<<<]>>>>>]<<<<<<<<<<
    [+>>>>>>>[-<<<<<<<+>>>>>>>[-<<<<<<<->>>>>>+>
             [-<<<<<<<+>>>>>>>[-<<<<<<<->>>>>>+>
             [-<<<<<<<+>>>>>>>[-<<<<<<<->>>>>>+>
             [-<<<<<<<+>>>>>>>[-<<<<<<<->>>>>>+>
             [-<<<<<<<+>>>>>>>]]]]]]]]]<<<<<<<
             [->>>>>>>+<<<<<<<]-<<<<<<<<<<]
    >>>>>>>
    [-<<<<<<<<<<<+>>>>>>>>>>>]
      >>>[>>>>>>>[-<<<<<<<<<<<+++++>>>>>>>>>>>]>>>]<<<<<<<<<<
    [+>>>>>>>>[-<<<<<<<<+>>>>>>>>[-<<<<<<<<->>>>>+>>>
              [-<<<<<<<<+>>>>>>>>[-<<<<<<<<->>>>>+>>>
              [-<<<<<<<<+>>>>>>>>[-<<<<<<<<->>>>>+>>>
              [-<<<<<<<<+>>>>>>>>[-<<<<<<<<->>>>>+>>>
              [-<<<<<<<<+>>>>>>>>]]]]]]]]]<<<<<<<<
              [->>>>>>>>+<<<<<<<<]-<<<<<<<<<<]
    >>>>>>>>[-<<<<<<<<<<<<<+>>>>>>>>>>>>>]>>
    [>>>>>>>>[-<<<<<<<<<<<<<+++++>>>>>>>>>>>>>]>>]<<<<<<<<<<
    [<<<<<<<<<<]>>>>>>>>>>
    >>>>>>
  ]
  <<<<<<

  *
  * make copies of the loop variable and the quotient
  *

  [>>>[->>>>+>+<<<<<]>>>>>>>]
  <<<<<<<<<<
  [>>>>>>>[-<<<<+>>>>]<<<<<[->>>>>+>>+<<<<<<<]<<<<<<<<<<<<]
  >>>>>>>>>>[>>>>>>>[-<<<<<+>>>>>]>>>]<<<<<<<<<<

  *
  * break out of the loop if the quotient is larger than the loop variable
  *

  [>>>>>>>>>[-<->]<
    [<<<<<<<<
      [<<[-]>>>>>>>>>>[-<<<<<<<<<<+>>>>>>>>>>]<<<<<<<<<<<<<<<<<<]
    >>>>>>>>>>>>>>>>>>]<<<<<<<<<<<<<<<<<<]
  >>>>>>>>[>-<[+[+[+[+[+[+[+[+[+[[-]>+<]]]]]]]]]]]>+

  [ [-]

    *
    * partially increment the loop variable
    *

    <[-]+>>>>+>>>>>>>>[>>>>>>>>>>]<<<<<<<<<<

    *
    * examine the remainder for nonzero digits
    *

    [<<<<<<[<<<<[<<<<<<<<<<]>>>>+<<<<<<<<<<]<<<<]
    >>>>>>>>>>>>>>>>>>>>[>>>>>>>>>>]<<<<<<<<<<[<<<<<<<<<<]
    >>>>-

    [ [+]

      *
      * decrement the loop variable and replace the number with the quotient
      *

      >>>>>>>>-<<[>[-]>>[-<<+>>]>>>>>>>]<<<<<<<<<<

      *
      * display the loop variable
      *

      [+>>[>>>>>>>>+>>]<<-<<<<<<<<<<]-
      [>>++++++++++++++++++++++++++++++++++++++++++++++++.
         ------------------------------------------------<<<<<<<<<<<<]
      ++++++++++++++++++++++++++++++++.[-]>>>>

    ]

    *
    * normalize the loop variable
    *

    >>>>>>
    [>>[->>>>>+<<<<<[->>>>>+<<<<<
       [->>>>>+<<<<<[->>>>>+<<<<<
       [->>>>>+<<<<<[->>>>>+<<<<<
       [->>>>>+<<<<<[->>>>>+<<<<<
       [->>>>>+<<<<<[->>>>>--------->>>>>+<<<<<<<<<<
       [->>>>>+<<<<<]]]]]]]]]]]>>>>>>>>]
    <<<<<<<<<<[>>>>>>>[-<<<<<+>>>>>]<<<<<<<<<<<<<<<<<]
    >>>>>>>>>

  ]<

]>>

*
* display the number and end
*

[>>>>>>>>>>]<<<<<<<<<<[+>[>>>>>>>>>+>]<-<<<<<<<<<<]-
[>++++++++++++++++++++++++++++++++++++++++++++++++.<<<<<<<<<<<]
++++++++++.
