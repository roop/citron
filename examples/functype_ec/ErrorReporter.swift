
class ErrorReporter {
    let inputString: String
    init(input: String) {
        inputString = input
    }
}

extension ErrorReporter : FunctionHeaderParser.CitronErrorCaptureDelegate {
    func shouldCaptureErrorOnParam(error: FunctionHeaderParser.UnexpectedTokenError,
        resolvedSymbols: [(name: String, value: Any)],
        unclaimedTokens: [(token: FunctionHeaderParser.CitronToken, tokenCode: FunctionHeaderParser.CitronTokenCode)],
        nextToken: (token: FunctionHeaderParser.CitronToken, tokenCode: FunctionHeaderParser.CitronTokenCode)?) -> CitronErrorCaptureResponse<FunctionParameter?> {
        let lastResolvedSymbolName = resolvedSymbols.last?.name
        let errorPosition = ((unclaimedTokens.first ?? nextToken)?.token)?.position
        reportErrorOnParam(lastResolvedSymbolName: lastResolvedSymbolName, errorPosition: errorPosition)
        return .captureAs(nil)
    }

}

extension ErrorReporter {
    func reportErrorOnParam(lastResolvedSymbolName: String?, errorPosition: Lexer.LexingPosition?) {

        if let lastResolvedSymbolName = lastResolvedSymbolName {
            switch (lastResolvedSymbolName) {
            case "local_param_name": fallthrough
            case "external_param_name":
                croak(expected: "':' after parameter name", pointAt: errorPosition)
            case "Colon":
                croak(expected: "type identifier or 'inout' after ':'", pointAt: errorPosition)
            case "KeywordInout":
                croak(expected: "type identifier after 'inout'", pointAt: errorPosition)
            default:
                croak(expected: "',' or ')' after parameter specification", pointAt: errorPosition)
            }
        } else {
            croak(expected: "parameter specification", pointAt: errorPosition)
        }

    }

    func croak(expected message: String, pointAt position: Lexer.LexingPosition?) {
        let line: Substring
        let padding: String
        if let position = position {
            print("\(position.lineNumber): error: expected \(message).")
            let endOfLine = inputString.endOfLine(from: position.linePosition)
            let column = inputString.distance(from: position.linePosition, to: position.tokenPosition)
            line = inputString[position.linePosition ..< endOfLine]
            padding = String(repeating: " ", count: column)
        } else {
            print("error: expected \(message).")
            let startOfLastLine = inputString.startOfLastLine()
            line = inputString[startOfLastLine ..< inputString.endIndex]
            padding = String(repeating: " ", count: line.count)
        }
        print(line)
        print("\(padding)^")
    }
}

private extension String {
    func endOfLine(from: String.Index) -> String.Index {
        var index = from
        while (index < self.endIndex && self[index] != "\n") {
            index = self.index(after: index)
        }
        return index
    }

    func startOfLastLine() -> String.Index {
        var index = self.endIndex
        while (index > self.startIndex && self[self.index(before: index)] != "\n") {
            index = self.index(before: index)
        }
        return index
    }
}
