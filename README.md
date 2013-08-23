Brainfuch
=========

Brainfuck interpreter written in Haskell. Features:

- Infinite data tape in both directions (thanks to laziness)
- Simple optimizations (e.g. combining multiple `+`)

The core part was written in roughly an hour without any prior knowledge of Brainfuck or interpretation of procedural languages. Turns out Brainfuck is really as simple as they all say.

Here's how to run a program:

```haskell
import Brainfuch
readFile "source.bf" >>= runBrainfuck
```

Note that the current version is pretty slow.

Licence
-------

WTFPL. http://www.wtfpl.net/txt/copying/