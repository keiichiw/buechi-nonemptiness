# nonemptiness check of Büchi automata

## Requirement
* OCaml 4.02.1
* ocamllex 4.02.1
* ocamlyacc 4.02.1
* ocamlbuild 4.02.1

## How to use

```
$ make
$ ./main.native ./sample/nonempty1.txt
nonempty!
./sample/nonempty1.dot created!
$ dot -Tpng ./sample/nonempty1.dot -o nonempty1.png
```

## Syntax of input file

```
<automaton>  ::= <trans> <inits> <acceptings>
<trans>      ::= {<state> <alpha> "->" <state>}
<inits>      ::= "S0:" {<state>}
<acceptings> ::= "F:"  {<state>}
<state>      ::= <alpha> (<alpha> | <digit>)+
<alpha>      ::= 'a'|..|'z'|'A'|..|'Z'|'_'
<digit>      ::= '0'|..|'9'
```
