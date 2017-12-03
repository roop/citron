---
title: "The Parser Interface"
permalink: /parser-interface/
---

_This documentation page is not fully baked yet._

# The Parser Interface

## The parser interface

The Citron-generated code contains a parser class that implements the
`CitronParser` protocol. The name of the parser class is `Parser` by
default, but [can be changed](/citron/grammar-file/#class_name).

The `CitronParser` protocol, defined in `CitronParser.swift`, implements
these two parsing methods:

 1. `consume(token:code:)`

    This methods makes the parser consume one token. This should be
    called multiple times to pass a sequence of tokens to the parser.
    Typically, a separate tokenization stage would generate this
    sequence of tokens. As the tokens get generated, they can be passed
    to the parser through this method.

    The first argument, `token:`, is the semantic value of the token, as
    seen by the code blocks in the grammar. The type of this argument is
    the [%token_type](/citron/grammar-file/#token_type) type specified
    in the grammar file, available to the parser class as the
    associated type `CitronToken`.

    The second argument, `code:`, is the token code that Citron knows
    this token by. The Citron-generated parser class contains an enum
    called `CitronTokenCode` that lists all the terminals in the
    grammar.  This argument should be a value of that enum.

 2. `endParsing()`

    This method tells the parser that there are no more tokens to
    consume, and signifies the end of input.

    This method takes no arguments, and returns the parse result. The
    parse result is the value returned by the code block of a start
    rule (a.k.a. root rule) of the grammar. If we're building a parse
    tree from the input as illustrated above, the parse result would
    typically be the completely built parse tree.

    The return type of this method is the
    [%nonterminal_type](/citron/grammar-file/#nonterminal_type) of the
    start symbol (a.k.a. root symbol) of the grammar.

### Error handling

Both `consume(token:code:)` and `endParsing()` are throwing methods.
They throw when an input token at a certain position is inconsistent
with the grammar, or when the input ends prematurely.

Moreover, we can throw errors from within a rule's code block and those
throws would propagate up to one of the two parsing methods.

## The lexer interface

Citron offers a simple lexer that can tokenize an input string.

We give the lexer a series of rules with either string or regular
expression patterns. The token data that the lexer should output can be
of an arbitrary type, so the lexer is defined as a generic type, with
the token data as a type parameter. For each string pattern, we should
associate it with the token data that should be output, and for each
regular expression pattern, we should provide a closure that returns the
token data from the matched string.

## Examples

A few examples of how Citron is used for parsing can be found in the
["examples" folder][eg] in the project repository.

[eg]: https://github.com/roop/citron/tree/master/examples/

