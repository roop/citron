---
title: "CitronErrorCaptureState"
permalink: /parser-interface/api/CitronErrorCaptureState/
layout: default

---

[Citron] > [Parser interface] > [`CitronErrorCaptureState`]

[Citron]: /citron/
[Parser interface]: /citron/parser-interface/
[`CitronErrorCaptureState`]: .

# CitronErrorCaptureState

_Structure_

A structure populated by Citron to pass the error capturing state to
[`CitronErrorCaptureDelegate` methods].

In case an error has been previously [saved] for capturing, and when
Citron has found a matching [synchronization point] for capturing onto
some [error-capturing non-terminal], this structure describes the
partially resolved state of that non-terminal.

The type of this struct should be accessed through the [`CitronErrorCaptureState`
associated type] on [`CitronParser`].

[`CitronErrorCaptureDelegate`]: /citron/parser-interface/api/CitronErrorCaptureDelegate/
[`CitronErrorCaptureDelegate` methods]: /citron/parser-interface/api/CitronErrorCaptureDelegate/#shouldcaptureerroronnameofnonterminalstate-citronerrorcapturestate-error-error
[saved]: /citron/parser-interface/api/CitronErrorCaptureDelegate/#shouldsaveerrorforcapturingerror-error
[synchronization point]: /citron/error-capturing/#2-specify-synchronization-points
[error-capturing non-terminal]: /citron/error-capturing/#1-enable-error-capturing

[`CitronParser`]: ../CitronParser/
[`CitronErrorCaptureState` associated type]: ../CitronParser/#citronerrorcapturestate
[`CitronSymbolCode`]: ../CitronParser/#citronsymbolcode
[`CitronToken`]: ../CitronParser/#citrontoken
[`CitronTokenCode`]: ../CitronParser/#citrontokencode

[`CitronErrorCaptureDelegate` associated type]: ../CitronParser/#citronerrorcapturedelegate
[%capture_errors]: /citron/grammar-file/#capture_errors
[grammar file]: /citron/grammar-file/
[class name]: /citron/grammar-file/#class_name

  - [Stored properties](#stored-properties)
      - [`resolvedSymbols: [(symbolCode: CitronSymbolCode, value: Any)]`](#resolvedsymbols-symbolcode-citronsymbolcode-value-any)
      - [`unclaimedTokens: [(token: CitronToken, tokenCode: CitronTokenCode)]`](#unclaimedtokens-token-citrontoken-tokencode-citrontokencode)
      - [`nextToken: (token: CitronToken, tokenCode: CitronTokenCode)?`](#nexttoken-token-citrontoken-tokencode-citrontokencode)
  - [Computed properties](#computed-properties)
      - [`lastResolvedSymbol: (symbolCode: CitronSymbolCode, value: Any)?`](#lastresolvedsymbol-symbolcode-citronsymbolcode-value-any)
      - [`erroringToken: (token: CitronToken, tokenCode: CitronTokenCode)?`](#erroringtoken-token-citrontoken-tokencode-citrontokencode)
  - [Example](#example)

[`resolvedSymbols`]: #resolvedsymbols-symbolcode-citronsymbolcode-value-any
[`unclaimedTokens`]: #unclaimedtokens-token-citrontoken-tokencode-citrontokencode
[`nextToken`]: (#nexttoken-token-citrontoken-tokencode-citrontokencode)

---

## Stored properties

### `resolvedSymbols: [(symbolCode: `[`CitronSymbolCode`]`, value: Any)]`

An array of the constitient symbols of the error-capturing non-terminal
that have been resolved so far. The `symbolCode` fields indicate which
symbol it is, and the `value` field stores the semantic value of that
symbol.

### `unclaimedTokens: [(token: `[`CitronToken`]`, tokenCode: `[`CitronTokenCode`]`)]`

The tokens that were passed in since the error occurred and till the
synchronization point was found.

If there was no error, these tokens would have been resolved, and along
with the `resolvedSymbols`, would have formed the complete
error-capturing non-terminal.

In case the [synchronization point] was matched using an `end_after`
clause, the `unclaimedTokens` sequence would end with the matching entry
(either a token or a sequence of tokens) in the `end_after` clause.

### `nextToken: (token: `[`CitronToken`]`, tokenCode: `[`CitronTokenCode`]`)?`

The current look-ahead token, at the time of attempting error capturing.

In case the [synchronization point] was matched using an `end_before`
clause, the `nextToken` would be the matching token in the `end_before`
clause.

In case the error is being captured at the end of input, `nextToken`
would be `nil`.

## Computed properties

### `lastResolvedSymbol: (symbolCode: `[`CitronSymbolCode`]`, value: Any)?`

The last entry in the [`resolvedSymbols`] array. In case [`resolvedSymbols`] is empty, this is `nil`.

### `erroringToken: (token: `[`CitronToken`]`, tokenCode: `[`CitronTokenCode`]`)?`

The first entry in the [`unclaimedTokens`] array. In case
[`unclaimedTokens`] is empty, this is the same as `nextToken`.

## Example

For example, if the [grammar file] contains these lines for a `param`
non-terminal representing a Swift function parameter:

~~~ Text
param ::= external_param_name local_param_name type_annotation.
type_annotation ::= Colon type.
type ::= Identifier.

%capture_errors param
    end_before(Comma | CloseBracket).
~~~

Consider the following input to the parser:

**func isOdd(number n Int) -> Bool**

Let's assume that the grammar identifies the `param` non-terminal
to start with the **number** token, and also identifies the
**number** token as an `external_param_name`, and the **n** token as a
`local_param_name`. The grammar expects a `Colon` token as the next
token, but it gets **Int**, causing a syntax error.

With error capturing turned on, Citron will save this error and start
looking for a synchronization point to capture this error. When it sees
the **)** in the input, or a `CloseBracket` token, it has a match in the
`end_before` clause of the `param` non-terminal.

At this point, Citron will call the [`shouldCaptureErrorOnParam`] method
of the [`errorCaptureDelegate`] object with the `state` argument set to
a `CitronErrorCaptureState` object with the following conceptual values:

  - [`resolvedSymbols`]:

     1. Symbol `external_param_name` with value **number**
     2. Symbol `local_param_name` with value **n**

  - [`unclaimedTokens`]:

     1. Token representing **Int** with code `Identifier`

  - [`nextToken`]:

     1. Token representing **)** with code `CloseBracket`

[`shouldCaptureErrorOnParam`]: ../CitronErrorCaptureDelegate/#func-shouldcaptureerroronnameofnonterminalstate-citronerrorcapturestate-error-error
[`errorCaptureDelegate`]: ../CitronParser/#errorcapturedelegate-citronerrorcapturedelegate
