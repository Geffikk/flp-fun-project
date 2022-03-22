# CFG-CNF (Functional project)
#### Author:
Maroš Geffert \<xgeffe00@stud.fit.vutbr.cz\>

## Build
Using `Makefile`
```bash
$ make build
$ make test
$ make clean
```
Using `Stack build`
```bash
$ stack build flp-fun-xgeffe00
```

## Running the program
```bash
$ ./bkg-2-cnf [-i|-1|-2] inputFile
```
`-i` - Option parse input grammar representation into Data type and print it to the output

`-1` - Option remove non simple rules from input grammar and print it to the output

`-2` - Option create CNF grammar and print it to the output

`inputFile` - *Optional arguments - Path for input grammar. If argument is not given, stdin will be used as input

## Known bugs
None - Everything should work properly

## Limitations
Grammar have to be in this format

*`Nonterminals` - Single symbol nonterminals [A-Z], separed by comma
```text
S,A,B,D
```
*`Terminals` - Only single symbols [a-z], separated by comma
```text
a,b,c,d
```
*`Starting symbol` - Have to be one of provided nonterminals.
```text
S
```

*`Rule` - [A-Z] -> [a-zA-Z] format.
```text
S -> Sa
S -> A
A -> Bc
B -> ac
Rule_n
```
