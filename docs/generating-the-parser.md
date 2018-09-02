---
title: "Generating the Parser"
permalink: /generating-the-parser/
layout: default

---

[Citron] > [Generating the parser]

[Citron]: /citron/
[Generating the parser]: .

# Generating the parser

To generate the Swift parser code from the input [grammar
file](/citron/grammar-file/), we should:

  - Get a compiled version of Citron
  - Run Citron on the grammar

That would create a Swift file containing the parser class.

## Citron source code

The Citron source code is just three files:

 1. **`citron.c`**: The Citron parser generator in C. This contains the
    code to generate a parser class in Swift that implements the
    `CitronParser` protocol.
 
 2. **`CitronParser.swift`**: This file defines the `CitronParser`
    protocol, which declares placeholders for the parser tables and
    implements parser methods that can work on the declared tables.

 3. **`CitronLexer.swift`**: This file defines the `CitronLexer` class,
    which can be used to generate the input for the parser.

While compiling our Swift code that uses [the parser
interface](/citron/parser-interface/), we should compile the following
along with that:

  - The generated parser
  - `CitronParser.swift`
  - `CitronLexer.swift`, if we're using the lexer

## Compiling Citron

To get a compiled version of Citron, we can run:

~~~ Text
clang citron.c -o citron
~~~

## Running Citron

To run Citron on an input grammar file called "grammar.y", we can run:

~~~ Text
citron grammar.y
~~~

which will generate an output file called `grammar.swift`. We can change
the output filename with the `-o` switch:

~~~ Text
citron grammar.y -o Parser.swift
~~~
