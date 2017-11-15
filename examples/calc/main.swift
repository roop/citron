
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

var parser = ArithmeticExpressionParser()
do {
    try parser.consume(token: 1, code: .INTEGER)
    try parser.consume(token: 0, code: .ADD)
    try parser.consume(token: 2, code: .INTEGER)
    try parser.consume(token: 0, code: .MULTIPLY)
    try parser.consume(token: 3, code: .INTEGER)
    let tree = try parser.endParsing()
    print("\(tree)")
} catch {
    print("Error during parsing")
}


