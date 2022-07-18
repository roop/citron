import CitronParserModule
import CitronLexerModule

class ErrorReporter {
    let inputString: String
    var endOfInputPosition: CitronLexerPosition?
    var isErrorReportedAtEnd: Bool = false
    init(input: String) {
        inputString = input
    }
}

extension ErrorReporter : FunctionHeaderParser.CitronErrorCaptureDelegate {
    typealias ECState = FunctionHeaderParser.CitronErrorCaptureState
    typealias ECResponse = CitronErrorCaptureResponse

    /* func_header */
    func shouldCaptureErrorOnFunc_header(state: ECState, error: Error) -> ECResponse<FunctionHeader?> {
        reportError(error: error, on: .func_header, state: state)
        return .captureAs(nil)
    }
    /* func_signature */
    func shouldCaptureErrorOnFunc_signature(state: ECState, error: Error) -> ECResponse<([FunctionParameter?]?, FunctionHeader.Throwability, TypeIdentifier)?> {
        reportError(error: error, on: .func_signature, state: state)
        return .captureAs(nil)
    }

    /* func_keyword_name */
    func shouldCaptureErrorOnFunc_keyword_name(state: ECState, error: Error) -> ECResponse<TypeIdentifier?> {
        reportError(error: error, on: .func_keyword_name, state: state)
        return .captureAs(nil)
    }

    /* param_clause */
    func shouldCaptureErrorOnParam_clause(state: ECState, error: Error) -> ECResponse<[FunctionParameter?]?> {
        reportError(error: error, on: .param_clause, state: state)
        return .captureAs(nil)
    }

    /* param */
    func shouldCaptureErrorOnParam(state: ECState, error: Error) -> ECResponse<FunctionParameter?> {
        reportError(error: error, on: .param, state: state)
        return .captureAs(nil)
    }

}

extension ErrorReporter {
    func reportError(error: Error, on errorCapturingSymbol: FunctionHeaderParser.CitronNonTerminalCode, state: ECState) {
        if (state.nextToken == nil) {
            // Report only one error while at the end of input
            if (isErrorReportedAtEnd) {
                return
            } else {
                isErrorReportedAtEnd = true
            }
        }

        let lastResolvedSymbolCode = state.lastResolvedSymbol?.symbolCode
        let errorPosition: CitronLexerPosition
        if case CitronLexerError.noMatchingRuleAt(let pos) = error {
            errorPosition = pos
        } else {
            errorPosition = state.erroringToken?.token.position ?? endOfInputPosition!
        }

        switch (errorCapturingSymbol) {
        case .func_header:
            switch (lastResolvedSymbolCode) {
            case .some(.func_signature):
              croak("extra characters after function header", at: errorPosition)
            default:
                // Can't really happen?
                croak("error in specifying function header", at: errorPosition)
            }
        case .func_keyword_name:
            switch (lastResolvedSymbolCode) {
            case .some(.funcHeaderKeywordFunc):
                croak("expected function name after 'func'", at: errorPosition)
            default:
                croak("expected 'func' keyword", at: errorPosition)
            }
        case .param_clause:
            switch (lastResolvedSymbolCode) {
            case .some(.funcHeaderOpenBracket):
                croak("expected parameter", at: errorPosition)
            default:
                croak("expected parameters list enclosed in (parantheses), or just '()' for an empty list", at: errorPosition)
            }
        case .func_signature:
            switch (lastResolvedSymbolCode) {
            case .some(.param_clause):
                croak("expected '->' or 'throws' or 'rethrows' or end of function header", at: errorPosition)
            case .some(.funcHeaderKeywordThrows): fallthrough
            case .some(.funcHeaderKeywordRethrows):
                croak("expected '->'", at: errorPosition)
            case .some(.funcHeaderArrow):
                croak("expected result type", at: errorPosition)
            default:
                // Can't really happen?
                croak("error in specifying return type", at: errorPosition)
            }
        case .param:
            switch (lastResolvedSymbolCode) {
            case .some(.local_param_name): fallthrough
            case .some(.external_param_name): fallthrough
            case .some(.funcHeaderIdentifier):
                croak("expected ':' after parameter name", at: errorPosition)
            case .some(.funcHeaderColon):
                croak("expected type identifier or 'inout' after ':'", at: errorPosition)
            case .some(.funcHeaderKeywordInout):
                croak("expected type identifier after 'inout'", at: errorPosition)
            case .some(.param_list):
                croak("expected ',' or ')' after parameter specification", at: errorPosition)
            default:
                croak("expected parameter specification", at: errorPosition)
            }
        default:
            fatalError()
        }
    }

    func croak(_ message: String, at position: CitronLexerPosition) {
        print("\(position.lineNumber): error: \(message).")
        let endOfLine = inputString.endOfLine(from: position.linePosition)
        let column = inputString.distance(from: position.linePosition, to: position.tokenPosition)
        let line = inputString[position.linePosition ..< endOfLine]
        let padding = String(repeating: " ", count: column)
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
