# Project review

## Task
Make turtle DSL, script interpreter for this DSL.
DSL should have variable, loops, default turtle commands
(forward, left, right, penup etc.)

## Architecture
### Files
- flake.nix - dev-shell for required version of haskell and stack
- example/ - directory with example(s) of DSL that can be executed
- tree-sitter/ - grammar description for parser generator treesitter
required to have syntax highlighting in nvim
- turtle-dsl/ - project
    - src/ - source folder
        - Parser.hs - module that parses pure text to haskell data 
        structure using megaparsec library
        - Interpreter.hs - module that interpretes haskell data 
        structures to `World` that represents state of canvas,
        position of turtle and other info
        - Main.hs - main module that consumes args from cli
        and call other modules
    - stack.yaml and turtle-dsl.cabal - configuration files that 
    define used libraries (megaparsec), executables and other
    information important to launch project

### DSL
First line could be of the form `NxM`, where `N` and `M` are 
integer values. `NxM` is size of field, that turtle has access to. 
If first line isn't of that form then it's parsed like regular command.
Important term:
- Expression (or expr) is either variable (any string satisfying 
`[a-zA-Z][a-zA-Z0-9]*`) or integer number

Language specification:
- `let x = 5` - while x isn't redefined, x will be replaced by 5
- `repeat n [ Command* ]` - Command* are repeated n times
- `forward expr` - turtle is going forward by `expr` squares,
if turtle is stuck on side, it just stands on same spot
- `left` and `right` - turn on 90 degrees
- `penup` and `pendown` - while pen is down, any movement will 
leave that trace: `#`

### Why flake.nix?
I couldn't start a project for several days, because couldn't get 
my environment to work with stack, ghc and other things. So, I'm writing 
that report and whole project in nvim on some server with NixOS (my laptop's
package manager now is nix too). 

### Why treesitter?
Because of vim

### Why megaparsec?
I wanted to use parsec, but then someone on Reddit said that megaparsec is 
"a fork of and fixes / refines / improves upon Parsec in multiple ways". I
really don't know why is it better, but it sound cool, using product-ready
libraries

### Why String, not Text?
Because my code consumes charaacter in strict order, so there is no need to 
use overloaded strings in tests. And it's simpler

## Performance
It works pretty fast, I guess

## Example
File `ex.turtle` outputs that masterpiece:
```masterpiece
#...................
.#..................
..#.................
...#................
....#...............
.....#..............
......#.............
.......#............
........#...........
.........#..........
..........#.........
...........#........
............#.......
.............#......
..............#.....
...............#....
................#...
.................#..
..................#.
...................#
```

