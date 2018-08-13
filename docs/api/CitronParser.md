---
title: "CitronParser"
permalink: /parser-interface/api/CitronParser/
---

# CitronParser (Protocol)

Defines the Citron parser interface.

## Parsing

### `consume(token: `[`CitronToken`]`, tokenCode: `[`CitronTokenCode`]`)`

Consumes one token.

This should be called multiple times to pass a sequence of tokens to the
parser. Typically, a separate tokenization stage would generate this
sequence of tokens.

Parameters:

  - `token`

    The semantic value of the token, as seen by the code blocks in the grammar.
    
    The type of this argument is the
    [%token_type](/citron/grammar-file/#token_type) type specified in
    the grammar file, available to the parser class as the associated
    type [`CitronToken`].

  - `code`
  
    The token code that Citron knows this token by.
    
    The Citron-generated parser class contains an enum called
    [`CitronTokenCode`] that lists all the terminals in the grammar.
    This argument should be a value of that enum.

Return value:

  - Does not return a value.

Throws:

  - Throws

### `endParsing()`

Signifies the end of input. This should be called when there are no more
tokens to consume.

Parameters:

  - This method takes no arguments
  
Return value:

  - Returns the parse result.
  
    The parse result is the value returned by the code block of a start
    rule (a.k.a. root rule) of the grammar.  If we're building a parse
    tree from the input as illustrated above, the parse result would
    typically be the completely built parse tree.

    The return type of this method is the
    [%nonterminal_type](/citron/grammar-file/#nonterminal_type) of the
    start symbol (a.k.a. root symbol) of the grammar, available to the
    parser class as the associated type [`CitronResult`].

## Errors

## Stack size

## Tracing

## Associated Types

### CitronToken
### CitronTokenCode
### CitronNonTerminalCode
### CitronSymbolCode
### CitronResult

[`CitronToken`]: #citrontoken
[`CitronTokenCode`]: #citrontokencode
[`CitronResult`]: #citronresult

