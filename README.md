Brainfuch
=========

Brainfuck interpreter written in Haskell. Features:

- Infinite data tape in both directions (thanks to laziness)
- Simple optimizations:
  - Multiple `<` and `>`, '+' and '-', '.' are combined into one instruction
  - Loops are their own primitive so jumping to the beginning/end is O(1)
  - All but the first consecutive loop are dropped, and so are loops at the beginning of the source (as the pivot is zero in these scenarios, hence the loops are never entered)

The core part was written in roughly an hour without any prior knowledge of Brainfuck or interpretation of procedural languages. Turns out Brainfuck is really as simple as they all say.

Note that the current version is probably not very performant. (Haven't compared it to anything though.)

Licence
-------

WTFPL. http://www.wtfpl.net/txt/copying/