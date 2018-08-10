
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
                return (number, .INTEGER)
            }
            return nil
        }),

        // Operators

        .string("+", (0, .ADD)),
        .string("-", (0, .SUBTRACT)),
        .string("*", (0, .MULTIPLY)),
        .string("/", (0, .DIVIDE)),

        // Brackets

        .string("(", (0, .OPEN_BR)),
        .string(")", (0, .CLOSE_BR)),

        // Whitespace

        .regexPattern("\\s", { _ in nil })
    ])

// Error capturing

class ErrorCapturer: ArithmeticExpressionParser.CitronErrorCaptureDelegate {
    var inputString: String = ""
    weak var lexer: Lexer? = nil

    func shouldSaveErrorForCapturing(error: Error) -> Bool {
        let errorPosition: String.Index
        if case CitronLexerError.noMatchingRuleAt(let pos) = error {
            errorPosition = pos.tokenPosition
        } else {
            errorPosition = lexer!.currentPosition.tokenPosition
        }
        print("Error: Unexpected input after '\(inputString.prefix(upTo: errorPosition))'")
        return true
    }

    func shouldCaptureErrorOnRoot(state: ArithmeticExpressionParser.CitronErrorCaptureState,
        error: Error) -> CitronErrorCaptureResponse<ArithmeticExpression> {
        return .captureAs(.error(error))
    }

    func shouldCaptureErrorOnExpr(state: ArithmeticExpressionParser.CitronErrorCaptureState,
        error: Error) -> CitronErrorCaptureResponse<ArithmeticExpression> {
        return .captureAs(.error(error))
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
