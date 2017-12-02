/*
    Copyright (C) 2017 Roopesh Chander <roop@roopc.net>

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import Foundation

class CitronLexer<TokenData> {
    typealias Action = (TokenData) throws -> Void
    enum LexingRule {
        case string(String, TokenData?)
        case regex(NSRegularExpression, (String) -> TokenData?)
        case regexPattern(String, (String) -> TokenData?)
    }
    let rules: [LexingRule]

    init(rules: [LexingRule]) {
        self.rules = rules.map { rule in
            // Convert .regexPattern values to equivalent .regex values
            switch (rule) {
            case .regexPattern(let pattern, let handler):
                return .regex(try! NSRegularExpression(pattern: pattern), handler)
            default:
                return rule
            }
        }
    }

    func tokenize(_ string: String, onFound: Action) throws {
        var index = string.startIndex
        while (index < string.endIndex) {
            var matched = false
            for rule in rules {
                switch (rule) {
                case .string(let ruleString, let tokenData):
                    if (string.suffix(from: index).hasPrefix(ruleString)) {
                        if let tokenData = tokenData {
                            try onFound(tokenData)
                        }
                        index = string.index(index, offsetBy: ruleString.count)
                        matched = true
                    }
                case .regex(let ruleRegex, let handler):
                    let result = ruleRegex.firstMatch(in: string, options: .anchored, range:
                        NSRange(
                            location: string.prefix(upTo: index).utf16.count,
                            length: string.suffix(from: index).utf16.count)
                    )
                    if let matchingRange = result?.range {
                        let start = string.utf16.index(string.utf16.startIndex, offsetBy: matchingRange.lowerBound)
                        let end = string.utf16.index(string.utf16.startIndex, offsetBy: matchingRange.upperBound)
                        if let matchingString = String(string.utf16[start..<end]) {
                            if let tokenData = handler(matchingString) {
                                try onFound(tokenData)
                            }
                            index = string.index(index, offsetBy: matchingString.count)
                            matched = true

                        }
                    }
                default:
                    fatalError("Internal error")
                }
                if (matched) {
                    break
                }
            }
            if (!matched) {
                throw CitronLexerError.noMatchingRuleAt(index: index, in: string)
            }
        }
    }
}

enum CitronLexerError: Error {
    case noMatchingRuleAt(index: String.Index, in: String)
}
