---
title: "The Parsing Interface"
permalink: /parsing-interface/
layout: default

---

[Citron] > [Parsing interface]

[Citron]: /citron/
[Parsing interface]: .

# The Parsing Interface

- [Summary of the interface](#summary-of-the-interface)
   - [Parser interface](#parser-interface)
   - [Lexer interface](#lexer-interface)
   - [Error handling interface](#error-handling-interface)
      - [Parsing](#parsing)
      - [Tokenization](#tokenization)
      - [Error capturing](#error-capturing)
- [Detailed API documentation](#detailed-api-documentation)
- [Examples](#examples)

## Summary of the interface

### Parser interface

The parser interface consists of these two methods which can be called
on the parser class [generated] by Citron:

  - [`consume(token:, code:)`]

    We call this method to pass a single token to the parser.

    We should call this method multiple times to pass a sequence of
    tokens to the parser. Typically, a separate tokenization stage (see
    [Lexer interface](#lexer-interface)) would generate this sequence of
    tokens.

  - [`endParsing()`]

    After passing all the tokens to the parser, we call this
    method to tell the parser that there are no more tokens to consume.

    This method returns the parse result, i.e. the value returned by the
    the code block of the [start rule] of the grammar.

    If we're building a parse tree from the input, the parse result
    would typically be the completely built parse tree.

These methods are actually defined on the [`CitronParser`] protocol,
which the Citron-generated parser class conforms to.

[generated]: /citron/generating-the-parser/
[start rule]: /citron/grammar-file/#start-rule

### Lexer interface

Tokenization is a separate step that needs to drive the parsing. As the
tokens are generated from the input string or data, the tokens should be
passed to the parser's [`consume(token:, code:)`] method.

Citron provides a simple rule-based lexer, [`CitronLexer`], that can
generate these tokens from an input string. The important methods it
offers are:

  - [`init(rules:)`]

    We create the lexer with a list of lexing rules.

  - [`tokenize(string:, onFound:)`]

    The lexer tokenizes `string`, and calls the `onFound` block when
    tokens matching the rules are found.

[`CitronParser`]: api/CitronParser/#citronparser
[`consume(token:, code:)`]: api/CitronParser/#consumetoken-citrontoken-tokencode-citrontokencode
[`endParsing()`]: api/CitronParser/#endparsing

[`CitronLexer`]: api/CitronLexer/#citronlexer
[`init(rules:)`]: api/CitronLexer/#initrules-lexingrule
[`tokenize(string:, onFound:)`]: api/CitronLexer/#tokenize_-string-string-onfound-action

### Error handling interface

#### Parsing

Both [`consume(token:, code:)`] and [`endParsing()`] are throwing
methods.  They throw when an input token at a certain position is
inconsistent with the grammar, or when the input ends prematurely.

#### Tokenization

[`tokenize(string:, onFound:)`] throws on errors encountered during
tokenization.

#### Error capturing

For more advanced error handling capabilities, see [Error
capturing](/citron/error-capturing/).

## Detailed API documentation

  - [`CitronParser`](api/CitronParser/)
  - [`CitronLexer`](api/CitronLexer/)
  - [`CitronErrorCaptureDelegate`](api/CitronErrorCaptureDelegate/)
  - [`CitronErrorCaptureState`](api/CitronErrorCaptureState/)

## Examples

A few examples of how Citron is used for parsing can be found in the
["examples" folder][eg] in the project repository.

[eg]: https://github.com/roop/citron/tree/master/examples/

