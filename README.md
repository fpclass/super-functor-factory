# Super Functor Factory

This repository contains the skeleton code for the "Super Functor Factory" project, which comprises:

- Function stubs for all of the functions that need to be implemented as part of the project. The Super Functor Factory program can be run with `stack run`. In order to complete the project, only `src/Game.hs` needs to be modified.
- A test suite for the project, which can be run with `stack test`. The source code for the test suite can be found in `test/Spec.hs`.

## Running the program

You can use `stack run` to get Stack to build and run your program. Additionally, the following options are supported:

* `--no-unicode` will use colour-specific characters instead of lambdas to display the conveyor in the terminal.
* `--no-colour` implies `--no-unicode` and also will not use colour codes in the terminal.

When using Stack, the options can be invoked with e.g. `stack run -- --no-unicode`.
