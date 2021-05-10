# ComputorV1 [![Actions Status](https://github.com/RadioPotin/ft-turing42/workflows/build/badge.svg)](https://github.com/RadioPotin/ft-turing42/actions) [![coverage percentage](https://raw.githubusercontent.com/RadioPotin/ft-turing42/gh-pages/coverage/badge.svg)](https://RadioPotin.github.io/ft-turing42/coverage/)

# Building
```
dune build @all
```
creates an executable
```
_build/default/src/ft-turing.exe
```
to which you can feed a `.json` file in which a given Turing machine is defined as well as an input to feed to said Turing Machine.

# Testing
```
dune runtest
```
This will run all tests contained in the folder `test`.

# Usage

```
./_build/default/src/ft-turing.exe <turingmachine.json> <input>
```
`<input>` being the input to feed to the turing machine described in

`<turingmachine.json>` being a file that defines a turing machine as follows:

<ol>
  
**name:** The name of the described machine.

**alphabet:** Both input and work alphabet of the machine merged into a single alphabet for simplicity’s sake, including the blank character. Each character of the alphabet must be a string of length strictly equal to 1.

**blank:** The blank character, must be part of the alphabet, must **NOT** be part of the input.

**states:** The exhaustive list of the machine’s states names.

**initial:** The initial state of the machine, must be part of the states list.

**finals:** The exhaustive list of the machine’s final states. This list must be a sub-list of the states list.

**transitions:** A dictionnary of the machine’s transitions indexed by state name. Each transition is a list of dictionnaries, and each dictionnary describes the transition fora given character under the head of the machine. A transition is defined as follows:

<ol>
  
**read:** The character of the machine’s alphabet on the tape under the machine’shead.

**to_state:** The new state of the machine after the transition is done.

**write:** The character of the machine’s alphabet to write on the tape before moving the head.

**action:** Movement of the head for this transition, either LEFT, or RIGHT

</ol>

*WIP*

