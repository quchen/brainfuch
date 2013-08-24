Brainfuch
=========

Brainfuck interpreter written in Haskell. Features:

- Infinite data tape in both directions (thanks to laziness)
- Simple optimizations:
  - Multiple `<` and `>`, '+' and '-' are combined into one instruction (at parse time)
  - Loops are their own primitive so jumping to the beginning/end is O(1)

The core part was written in roughly an hour without any prior knowledge of Brainfuck or interpretation of procedural languages. Turns out Brainfuck is really as simple as they all say.

Here's how to run a program:

```haskell
import Brainfuch
main = readFile "source.bf" >>= run
```

Note that the current version is probably not very performant. (Haven't compared it to anything though.

Licence
-------

WTFPL. http://www.wtfpl.net/txt/copying/