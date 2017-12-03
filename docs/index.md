
# Citron Parser Generator

[Citron] is an [LALR] parser generator for Swift. For a given input
grammar, Citron creates a reentrant type-safe parser in Swift.

[LALR]: https://en.wikipedia.org/wiki/LALR_parser
[Citron]: https://github.com/roop/citron

## Features

  - **Swifty**

    Citron encapsulates parsing-related data and functions into a parser
    class, and uses protocols and generics to provide a Swifty interface
    for parsing and lexing.

  - **Reentrant**

    You can have different instances of a Citron-generated parser class
    in the same program, and the different instances can be used
    concurrently. You can also have different Citron-generated parser
    classes in the same program, and instances of those classes can be
    used concurrently.

    However, you should not access the same instance of a parser class
    from multiple threads at the same time (i.e. Citron-generated
    parsers are not thread-safe).

  - **Type-safe**

    The Citron-generated code enforces type checking on the inputs and
    outputs of every code block in the grammar file, ensuring that bugs
    are caught at build time rather than at runtime.

## Origins

Citron is adapted from the [Lemon] parser generator by [Richard Hipp],
the creator of SQLite. Lemon is used to generate the parser that parses
SQL statements in SQLite.

[Lemon]: https://www.hwaci.com/sw/lemon/lemon.html
[Richard Hipp]: http://www.hwaci.com/drh/

## Requirements

Using Citron requires Swift 4. The parser has no dependancies other than
the [Swift Standard Library][stdlib]. The lexer is dependant on
[Foundation][foundation] for the use of regular expressions.

[stdlib]: https://developer.apple.com/documentation/swift
[foundation]: https://developer.apple.com/documentation/foundation

## Using Citron

To make use of Citron, you should:

 1. Create a grammar file

    The grammar file contains the input grammar for which we'd like
    Citron to create a parser. It contains grammar rules, code blocks
    associated with the rules and Citron directives.

    See [_The Citron Grammar File_](grammar-file/) for information on how to write a
    grammar file.

 2. Generate the parser

    To generate a parser, you should compile Citron and then run Citron
    on the grammar file.

    See _Generating the parser_ for the commands that can accomplish
    this.

 3. Use the parser

    You can then use the parser class in your code, and provide it with
    inputs. You can optionally use Citron's lexer to generate the inputs
    for the parser.

    See _The Parsing Interface_ for information on how your code should
    use the parser.

