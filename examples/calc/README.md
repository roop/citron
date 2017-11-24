
This is an example of using Citron to generate a parser for a grammar.

The goal is to parse simple arithmetic expressions like "1 + 2 * 3" and
convert an expression like that into a parse tree. The parse tree
generated uses an enum that's similar to the `ArithmeticExpression` enum
described in [_Recursive Enumerations_][swift_doc_rec_enums] in the
Swift documentation.

The parse tree is printed out in prefix notation in a lisp-like format.

[swift_doc_rec_enums]: https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Enumerations.html#//apple_ref/doc/uid/TP40014097-CH12-ID536

