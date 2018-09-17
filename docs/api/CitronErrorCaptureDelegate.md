---
title: "CitronErrorCaptureDelegate"
permalink: /parser-interface/api/CitronErrorCaptureDelegate/
layout: default

---

[Citron] > [Parser interface] > [`CitronErrorCaptureDelegate`]

[Citron]: /citron/
[Parser interface]: /citron/parser-interface/
[`CitronErrorCaptureDelegate`]: .

# CitronErrorCaptureDelegate

_Protocol_

Defines the Citron error capturing interface.

To enable error capturing, we need to make a class in our code adopt
this protocol, and set the [`errorCaptureDelegate`] property of the
parser to an instance of that class.

The definition of this protocol, including its name, is automatically
generated. Therefore, instead of directly accessing it by name, this
protocol should be accessed through the [`CitronErrorCaptureDelegate`
associated type] on [`CitronParser`].

For example, if the generated parser [class name] is `MyParser`, and the
class adopting the protocol is `MyClass`, we can write:

~~~ Swift
extension MyClass: MyParser.CitronErrorCaptureDelegate {
    // protocol method implementations go here
}
~~~

[`CitronParser`]: ../CitronParser/
[`errorCaptureDelegate`]: ../CitronParser/#errorcapturedelegate-citronerrorcapturedelegate
[`CitronErrorCaptureDelegate` associated type]: ../CitronParser/#citronerrorcapturedelegate
[%capture_errors]: /citron/grammar-file/#capture_errors
[grammar file]: /citron/grammar-file/
[class name]: /citron/grammar-file/#class_name

  - [Methods to implement](#methods-to-implement)
      - [`shouldSaveErrorForCapturing(error: Error)`](#shouldsaveerrorforcapturingerror-error)
      - [<code>shouldCaptureErrorOn<b><i>NameOfNonTerminal</i></b>(state: CitronErrorCaptureState, error: Error)</code>](#shouldcaptureerroronnameofnonterminalstate-citronerrorcapturestate-error-error)

---

## Methods to implement

### `shouldSaveErrorForCapturing(error: Error)`

When an error occurs when an error-capturing non-terminal is being
parsed, Citron calls this method to ask our code if we want to save this
error and start looking for a [synchronization point] to capture this
error.

Implementing this method is optional -- the default implementation of this
method always returns `true`.

**Parameters:**

  - `error`
  
    The error that has just occurred.

**Return value:**

If we want Citron to save this error and start looking for a
[synchronization point] to capture this error, we should return `true`.

If we don't want to use error capturing for this error, we should return
`false`. In this case, the error will get thrown instead.

[synchronization point]: /citron/error-capturing/#synchronization-point

### <code>shouldCaptureErrorOn<b><i>NameOfNonTerminal</i></b>(state: <a href="../CitronErrorCaptureState/">CitronErrorCaptureState</a>, error: Error)</code>

When a matching [synchronization point] is found, Citron calls this
method to ask our code if we want to capture the error at this point. If
we do want to capture the error, our code is responsible for creating
the error-captured semantic value of the corresponding non-terminal and
return it.

The `CitronErrorCaptureDelegate` protocol contains a method like this
for every error-capturing non-terminal.

**Parameters:**

  - <span id="state-parameter">`state`</span>

    An instance of type [`CitronErrorCaptureState`], that contains the
    current error capturing state.

    This can be used to diagonize the cause of the error and report an
    appropriate error message. If we're trying to produce a partial
    parse tree, this can be used to create a node for that tree with
    partially parsed information.

  - `error`
  
    The error that we're trying to capture.

**Return value:**

<span id="citronerrorcaptureresponse">Should be a value of type
`CitronErrorCaptureResponse`, which is defined as follows, with `T`
being the [semantic type] of the error-capturing non-terminal:</span>

~~~ Swift
enum CitronErrorCaptureResponse<T> {
    case captureAs(T)
    case dontCapture
}
~~~

If we want to capture the error at this point, we should return
`.captureAs` with the error-captured semantic value as the associated
value. If we're trying to produce a partial parse tree, the semantic
value is typically the partially populated node representing the
error-capturing non-terminal in that tree.

If we don't want to capture the error at this point, we should return
`.dontCapture`. In this case, error capturing will be re-attempted when
the next synchronization point is found, or when the end of input is
reached.

[semantic type]: /citron/grammar-file/#types
[`CitronErrorCaptureState`]: ../CitronErrorCaptureState/

