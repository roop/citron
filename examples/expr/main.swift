import CitronLexerModule
import CitronParserModule

// Parse tree node type

enum ArithmeticExpression {
    case number(Int)
    indirect case addition(ArithmeticExpression, ArithmeticExpression)
    indirect case subtraction(ArithmeticExpression, ArithmeticExpression)
    indirect case multiplication(ArithmeticExpression, ArithmeticExpression)
    indirect case division(ArithmeticExpression, ArithmeticExpression)
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
        }
    }
}

// Create parser

let parser = ArithmeticExpressionParser()
// parser.isTracingEnabled = true

// Create lexer

typealias Lexer = CitronLexer<(Int, ArithmeticExpressionParser.CitronTokenCode)>

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

// Tokenize and parse

if CommandLine.argc != 2 {
    print("Pass the expression to be parsed as a quoted argument.")
} else {
    let inputString = CommandLine.arguments[1]
    do {
        try lexer.tokenize(inputString) { (t, c) in
            try parser.consume(token: t, code: c)
        }
        let tree = try parser.endParsing()
        print("\(tree)")
    } catch (let error) {
        print("Error during parsing: \(error)")
    }
}
