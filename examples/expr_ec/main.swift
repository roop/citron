
// Parse tree node type

enum ArithmeticExpression {
    case number(Int)
    indirect case addition(ArithmeticExpression, ArithmeticExpression)
    indirect case subtraction(ArithmeticExpression, ArithmeticExpression)
    indirect case multiplication(ArithmeticExpression, ArithmeticExpression)
    indirect case division(ArithmeticExpression, ArithmeticExpression)
    case error(Error)
}

extension ArithmeticExpression: CustomStringConvertible {
    var description: String {
        switch (self) {
        case .number(let v):
            return "\(v)"
        case .addition(let left, let right):
            return "(+ \(left) \(right))"
        case .subtraction(let left, let right):
            return "(- \(left) \(right))"
        case .multiplication(let left, let right):
            return "(* \(left) \(right))"
        case .division(let left, let right):
            return "(/ \(left) \(right))"
        case .error(_):
            return "[ERR]"
        }
    }
}

// Create parser

let parser = ArithmeticExpressionParser()
// parser.isTracingEnabled = true

// Create lexer

typealias Lexer = CitronLexer<(token: Int, code: ArithmeticExpressionParser.CitronTokenCode)>

let lexer = Lexer(rules: [

        // Numbers

        .regexPattern("[0-9]+", { str in
            if let number = Int(str) {
                return (number, .Integer)
            }
            return nil
        }),

        // Operators

        .string("+", (0, .Add)),
        .string("-", (0, .Subtract)),
        .string("*", (0, .Multiply)),
        .string("/", (0, .Divide)),

        // Brackets

        .string("(", (0, .OpenBracket)),
        .string(")", (0, .CloseBracket)),

        // Whitespace

        .regexPattern("\\s", { _ in nil })
    ])

// Error capturing

class ErrorCapturer: ArithmeticExpressionParser.CitronErrorCaptureDelegate {
    var inputString: String = ""
    weak var lexer: Lexer? = nil
    var errorPosition: CitronLexerPosition? = nil

    func shouldSaveErrorForCapturing(error: Error) -> Bool {
        if case CitronLexerError.noMatchingRuleAt(let pos) = error {
            errorPosition = pos
        } else {
            errorPosition = lexer!.currentPosition
        }
        return true
    }

    func shouldCaptureErrorOnRoot(state: ArithmeticExpressionParser.CitronErrorCaptureState,
        error: Error) -> CitronErrorCaptureResponse<ArithmeticExpression> {
        printError(capturingOn: .root, state: state, position: errorPosition!)
        return .captureAs(.error(error))
    }

    func shouldCaptureErrorOnExpr(state: ArithmeticExpressionParser.CitronErrorCaptureState,
        error: Error) -> CitronErrorCaptureResponse<ArithmeticExpression> {
        printError(capturingOn: .expr, state: state, position: errorPosition!)
        return .captureAs(.error(error))
    }

    func printError(capturingOn: ArithmeticExpressionParser.CitronNonTerminalCode,
            state: ArithmeticExpressionParser.CitronErrorCaptureState, position: CitronLexerPosition) {
        let msg: String
        if (capturingOn == .root &&
            state.erroringToken == nil &&
            state.resolvedSymbols.contains { $0.symbolCode == .OpenBracket }) {
            msg = "Parenthesis not closed"
        } else {
            switch (state.lastResolvedSymbol?.symbolCode) {
            case .some(.Integer): fallthrough
            case .some(.CloseBracket): fallthrough
            case .some(.expr):
                msg = "Expecting an operator: +, -, *, or /"
            default:
                msg = "Expecting an integer or parenthesized expression"
            }
        }
        print("Error: \(msg)")
        let endOfLine = inputString.endOfLine(from: position.linePosition)
        let column = inputString.distance(from: position.linePosition, to: position.tokenPosition)
        let line = inputString[position.linePosition ..< endOfLine]
        let padding = String(repeating: " ", count: column)
        print(line)
        print("\(padding)^")
    }

    func printState(_ msg: String, _ state: ArithmeticExpressionParser.CitronErrorCaptureState) {
        print("\(msg)")
        print("---")
        print("Resolved symbols:")
        for (i, rs) in state.resolvedSymbols.enumerated() {
            print("  \(i). \(rs.symbolCode) (value: \(rs.value))")
        }
        print("Unclaimed tokens:")
        for (i, ut) in state.unclaimedTokens.enumerated() {
            print("  \(i). \(ut.tokenCode) (value: \(ut.token))")
        }
        print("Next token:")
        if let nt = state.nextToken {
            print("  [non-nil]: \(nt.tokenCode) (value: \(nt.token))")
        } else {
            print("  [nil]")
        }
        print("---")
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
}

let errorCapturer = ErrorCapturer()
parser.errorCaptureDelegate = errorCapturer

// Tokenize and parse

if CommandLine.argc != 2 {
    print("Pass the expression to be parsed as a quoted argument.")
} else {
    let inputString = CommandLine.arguments[1]
    errorCapturer.inputString = inputString
    errorCapturer.lexer = lexer
    do {
        try lexer.tokenize(inputString,
            onFound: { t in
                try parser.consume(token: t.token, code: t.code)
            },
            onError: { e in
                try parser.consume(lexerError: e)
            })
        let tree = try parser.endParsing()
        print("\(tree)")
    }
    // We handle all errors through error capturing, so we
    // won't have any errors to catch here.
}
