struct FunctionParameter {
    let localName: String
    let externalName: String?
    let type: String
    let isInout: Bool
}

struct FunctionHeader {
    enum Throwability {
        case throwing
        case rethrowing
        case nonthrowing
    }
    let name: String
    let parameters: [FunctionParameter]
    let returnType: String
    let throwability: Throwability

    func typeString() -> String {
        return "("
                + parameters.map { p in
                    p.isInout ? "inout \(p.type)" : p.type
                }.joined(separator: ", ")
                + (throwability == .nonthrowing ? ") -> " : ") throws -> ")
                + returnType
    }
}

func parseFunctionHeader(input: String) -> FunctionHeader? {

    // Create parser
    let parser = FunctionHeaderParser()

    // Create lexer
    typealias Lexer = CitronLexer<(FunctionHeaderParser.CitronToken,
                        FunctionHeaderParser.CitronTokenCode)>
    let lexer = Lexer(rules: [

            // Keywords

            .string("func", (.keyword, .FUNC)),
            .string("throws", (.keyword, .THROWS)),
            .string("rethrows", (.keyword, .RETHROWS)),
            .string("inout", (.keyword, .INOUT)),

            // Punctuations

            .string("->", (.punctuation, .FUNC_ARROW)),
            .string("(", (.punctuation, .L_BR)),
            .string(")", (.punctuation, .R_BR)),
            .string(",", (.punctuation, .COMMA)),
            .string(":", (.punctuation, .COLON)),

            // Identifiers

            .regexPattern("[a-zA-Z0-9_]+", { str in
                (.identifier(str), .IDENTIFIER)
            }),

            // Ignore whitespace

            .regexPattern("\\s", { _ in nil })
        ])

    // Tokenize and parse
    var funcHeader: FunctionHeader? = nil
    do {
        try lexer.tokenize(input) { (t, c) in
            try parser.consume(token: t, code: c)
        }
        funcHeader = try parser.endParsing()
    } catch CitronLexerError.noMatchingRuleAt(let index, let string) {
        print("Error during tokenization after '\(string.prefix(upTo: index))'.")
    } catch let error {
        print("Error during parsing: \(error)")
    }
    return funcHeader
}

if CommandLine.argc != 2 {
    print("Pass the function header to be parsed as a quoted argument.")
} else {
    if let funcHeader = parseFunctionHeader(input: CommandLine.arguments[1]) {
       print("Function type is: \(funcHeader.typeString())")
    }
}
