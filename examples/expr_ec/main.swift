import CitronLexerModule
import CitronParserModule

// Parse tree node type

enum ArithmeticExpression {
    case number(Int)
    indirect case addition(ArithmeticExpression, ArithmeticExpression)
    indirect case subtraction(ArithmeticExpression, ArithmeticExpression)
    indirect case multiplication(ArithmeticExpression, ArithmeticExpression)
    indirect case division(ArithmeticExpression, ArithmeticExpression)
    indirect case error(Error, ArithmeticExpression?)
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
        case .error(_, let e):
            if let e = e {
                return "\(e)?"
            } else {
                return " ?"
            }
        }
    }
}

// Create parser

let parser = ArithmeticExpressionParser()
// parser.isTracingEnabled = true

// Create lexer

typealias Lexer = CitronLexer<(token: Int, code: ArithmeticExpressionParser.CitronTokenCode)>

func makeLexerAndParser(input: String) -> (Lexer, ArithmeticExpressionParser) {

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

    let parser = ArithmeticExpressionParser()
    let errorCapturer = ErrorCapturer()
    errorCapturer.inputString = input
    errorCapturer.lexer = lexer
    parser.errorCaptureDelegate = errorCapturer
    return (lexer, parser)
}

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
        // printState("root", state)
        printError(state: state, position: errorPosition!)
        return .captureAs(.error(error, findPartialExpression(state: state)))
    }

    func shouldCaptureErrorOnExpr(state: ArithmeticExpressionParser.CitronErrorCaptureState,
        error: Error) -> CitronErrorCaptureResponse<ArithmeticExpression> {
        // printState("expr", state)
        printError(state: state, position: errorPosition!)
        return .captureAs(.error(error, findPartialExpression(state: state)))
    }

    func printError(state: ArithmeticExpressionParser.CitronErrorCaptureState, position: CitronLexerPosition) {
        let msg: String
        switch (state.lastResolvedSymbol?.symbolCode) {
        case .some(.Integer): fallthrough
        case .some(.CloseBracket): fallthrough
        case .some(.expr):
            if (state.erroringToken == nil &&
                state.resolvedSymbols.contains { $0.symbolCode == .OpenBracket }) {
                msg = "Parenthesis not closed"
            } else {
                msg = "Expecting an operator: +, -, *, or /"
            }
        default:
            msg = "Expecting an integer or parenthesized expression"
        }
        print("Error: \(msg)")
        let endOfLine = inputString.endOfLine(from: position.linePosition)
        let column = inputString.distance(from: position.linePosition, to: position.tokenPosition)
        let line = inputString[position.linePosition ..< endOfLine]
        let padding = String(repeating: " ", count: column)
        print(line)
        print("\(padding)^")
    }

    func findPartialExpression(state: ArithmeticExpressionParser.CitronErrorCaptureState) -> ArithmeticExpression? {
        for resolvedSymbol in state.resolvedSymbols {
            if (resolvedSymbol.symbolCode == .expr) {
                if let e = resolvedSymbol.value as? ArithmeticExpression {
                    return e
                }
            } else if (resolvedSymbol.symbolCode == .Integer) {
                if let n = resolvedSymbol.value as? Int {
                    return .number(n)
                }
            }
        }
        for unclaimedToken in state.unclaimedTokens {
            if (unclaimedToken.tokenCode == .Integer) {
                let n = unclaimedToken.token
                return .number(n)
            }
        }
        return nil
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

// Tokenize and parse

if CommandLine.argc != 2 {
    print("Pass the expression to be parsed as a quoted argument.")
} else {
    let inputString = CommandLine.arguments[1]
    let (lexer, parser) = makeLexerAndParser(input: inputString)

    do {
        try lexer.tokenize(inputString,
            onFound: { t in
                try parser.consume(token: t.token, code: t.code)
            },
            onError: { e in
                try parser.consume(lexerError: e)
            })
        let tree = try parser.endParsing()
        print("Prefix notation: \(tree)")
    }
    // We handle all errors through error capturing, so we
    // won't have any errors to catch here.
}
