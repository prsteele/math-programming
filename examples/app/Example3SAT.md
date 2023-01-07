# 3SAT

This is a literate Haskell file.

We want to solve a [3SAT](https://en.wikipedia.org/wiki/3SAT) problem
using the [`math-programming`](../math-programming/) and
[`math-programming-glpk`](../math-programming-glpk/) libraries. We
begin by importing these libraries.

```haskell

```

We want to make a command-line program that will read in a 3SAT
instance, and either conclude that it is unsatisfiable or output
a witness to satisfiability. Our input format will have one clause per
line, with each variable in the clause being
whitespace-separated. Variables with a leading `-` sign are negated.

```haskell



```
