enum Token {
    case keyword // for func, throws, inout, etc.
    case punctuation // for (, ), ->, etc.
    case identifier(String) // for identifiers
    func toIdentifierString() -> String? {
        if case .identifier(let id) = self {
            return id
        }
        return nil
    }
}

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
    let parameters: [FunctionParameter?]
    let returnType: String
    let throwability: Throwability

    func typeString() -> String {
        return "("
                + parameters.map { p in
                    p == nil ? "ERROR" :
                      (p!.isInout ? "inout \(p!.type)" : p!.type)
                }.joined(separator: ", ")
                + (throwability == .nonthrowing ? ") -> " : ") throws -> ")
                + returnType
    }
}

typealias Lexer = CitronLexer<(Token, FunctionHeaderParser.CitronTokenCode)>

func parseFunctionHeader(input: String) -> FunctionHeader? {

    // Create parser
    let parser = FunctionHeaderParser()

    // Create lexer
    let lexer = Lexer(rules: [

            // Keywords

            .string("func", (.keyword, .funcHeaderKeywordFunc)),
            .string("throws", (.keyword, .funcHeaderKeywordThrows)),
            .string("rethrows", (.keyword, .funcHeaderKeywordRethrows)),
            .string("inout", (.keyword, .funcHeaderKeywordInout)),

            // Punctuations

            .string("->", (.punctuation, .funcHeaderArrow)),
            .string("(", (.punctuation, .funcHeaderOpenBracket)),
            .string(")", (.punctuation, .funcHeaderCloseBracket)),
            .string(",", (.punctuation, .funcHeaderComma)),
            .string(":", (.punctuation, .funcHeaderColon)),

            // Identifiers

            .regexPattern("[a-zA-Z0-9_]+", { str in
                (.identifier(str), .funcHeaderIdentifier)
            }),

            // Ignore whitespace

            .regexPattern("\\s", { _ in nil })
        ])

    // Enable error capturing
    let errorReporter = ErrorReporter(input: input)
    parser.errorCaptureDelegate = errorReporter

    // Tokenize and parse
    var funcHeader: FunctionHeader? = nil
    do {
        try lexer.tokenize(input) { (t, c) in
            try parser.consume(token: (token: t, position: lexer.currentPosition), code: c)
        }
        funcHeader = try parser.endParsing()
    } catch CitronLexerError.noMatchingRuleAt(let index, let string) {
        print("Error during tokenization after '\(string.prefix(upTo: index))'.")
    } catch (let e as FunctionHeaderParser.UnexpectedTokenError) {
       print("Error during parsing: Unexpected token: \(e.tokenCode) (\(e.token))")
    } catch (is FunctionHeaderParser.UnexpectedEndOfInputError) {
        print("Error during parsing: Unexpected end of input")
    } catch (let error) {
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
