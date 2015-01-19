# FORTH

A micro compiler for the FORTH programming language

I wanted to learn more about FORTH and given how little info there is online
writing an interpreter seemed like a good way to learn something.

```ocaml

# Forth> "5 DUP +";;
- : token list = [INT 10]

# Forth> "5 DUMP 6 DUMP + DUMP 7 DUMP 8 DUMP + DUMP";;
[ 5 ]
[ 6 5 ]
[ 11 ]
[ 7 11 ]
[ 8 7 11 ]
[ 15 11 ]
- : unit = ()
```
