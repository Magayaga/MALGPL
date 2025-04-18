<p align="center">
  <a href="https://github.com/magayaga/MALGPL">
    <img src="assets/malgpl-background.png" alt="MALGPL Background Logo" width="100%" height="100%">
  </a>
</p>

<h1 align="center">MALGPL</h1>

**MALGPL**, short for the **Magayagaian Algorithmic Programming Language** is an imperative, reflective, generic, array, and meta-programming language.  It was designed and developed by [Cyril John Magayaga](https://github.com/magayaga), best known as the original author and developer of the **Hyzero**, **Xenly**, **BINPL**, and **LibreBASIC** programming languages.

MALGPL is influenced by the **Fortran**, **C**, **BASIC**, **ALGOL**, and **ML** programming languages. Cyril wrote, “Fortran is the world’s first high-level programming language but it is the longest whole life of programming.” It was written in **Fortran 90** and **C** programming languages.

## Filename extensions

| Filename extensions | Definition | Written in |
|:-:|:-:|:-:|
| `.mlg` | MALGPL's source filename written in the Fortran programming language | Fortran |
| `.mlgp` | MALGPL's source filename written in the C programming language | C |

## Keywords

### Keywords written in `Fortran 90`

| `WRITE` | `WRITENUM` | `VAR` | `COMMENT` | `BEGIN` | `END` | `FOR` | `NEXT` | `VARL` | `LET` |
|:-------:|:----------:|:-----:|:---------:|:-------:|:-----:|:-----:|:------:|:------:|:-----:|

### Keywords written in `C`

| `PRINT` | `PRINTLN` | `INPUT` | `INPUTLN` | `ERROR` | `ERRORLN` | `CONST` | `BEGIN` |
|:-------:|:---------:|:-------:|:---------:|:-------:|:---------:|:-------:|:-------:|
| **`END`** | **


### Keywords with `Scala` (written in `C`)

| `OBJECT` | `FOR` | `WHILE` |
|:--------:|:-----:|:-------:|

### Keywords with `Python` (written in `C`)
| `IF` | `ELIF` | `ELSE` |
|:----:|:------:|:------:|

## Examples

### "Hello, World!" program

Here’s a simple program to print “Hello, World!” written in MALGPL programming language:

```basic
WRITE("Hello, World!")
```

and 

```basic
BEGIN
    COMMENT “Hello, World!” program
    WRITE("Hello, World!")
END
```

It is similar to the “Hello, World!” program written in ALGOL 60 programming language:

```basic
BEGIN
    COMMENT “Hello, World!” program
    WRITE("Hello, World!")
END
```

### Comments
`COMMENT` can be used to explain MALGPL code, and to make it more readable.

```basic
COMMENT This is the comment
COMMENT "This is the comment!"
```

and

```basic
BEGIN
    COMMENT This is the comment
    COMMENT "This is the comment!"
END
```

### Arithmetic expressions
`WRITENUM` can be numbers and arithmetic expressions and `WRITE` can be messages and texts.

Here’s a simple program to print arithmetic expressions written in MALGPL programming language:

```basic
WRITENUM(9 + 12)
WRITENUM(4 - 6)
WRITENUM(12 * 3)
WRITENUM(10 / 4)
WRITENUM(4 ^ 3)
```

and 

```basic
BEGIN
    COMMENT Arithmetic expressions like addition, subtraction, multiplication, division, and power.
    WRITENUM(9 + 12)
    WRITENUM(4 - 6)
    WRITENUM(12 * 3)
    WRITENUM(10 / 4)
    WRITENUM(4 ^ 3)
END
```

### Variables

MALGPL Variables can be a `VAR` keyword that can be changed or modified like numbers and messages.

```basic
VAR name := "Hello, World!"
WRITE(\name)

VAR number := 9 + 4
VAR 6 := 12 + 3
WRITENUM(\number)
WRITENUM(\6)
```
and

```basic
BEGIN
    COMMENT Var to the Write
    VAR name := "Hello, World!"
    WRITE(\name)

    COMMENT Var to the Writenum
    VAR number := 9 + 4
    VAR 6 := 12 + 3
    WRITENUM(\number)
    WRITENUM(\6)
END
```

## Copyright

Copyright (c) 2024-2025 Cyril John Magayaga.
