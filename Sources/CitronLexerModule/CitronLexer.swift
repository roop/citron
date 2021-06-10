// SPDX-License-Identifier: MIT
// Copyright (C) 2017 Roopesh Chander <roop@roopc.net>
// This file is part of the Citron Lexer Module

import Foundation

public typealias CitronLexerPosition = (tokenPosition: String.Index, linePosition: String.Index, lineNumber: Int)

public class CitronLexer<TokenData> {
    public typealias Action = (TokenData) throws -> Void
    public typealias ErrorAction = (CitronLexerError) throws -> Void
    public enum LexingRule {
        case string(String, TokenData?)
        case regex(NSRegularExpression, (String) -> TokenData?)
        case regexPattern(String, (String) -> TokenData?)
    }
    private let rules: [LexingRule]

    public private(set) var currentPosition: CitronLexerPosition

    public init(rules: [LexingRule]) {
        self.rules = rules.map { rule in
            // Convert .regexPattern values to equivalent .regex values
            switch (rule) {
            case .regexPattern(let pattern, let handler):
                return .regex(try! NSRegularExpression(pattern: pattern), handler)
            default:
                return rule
            }
        }
        currentPosition = (tokenPosition: "".startIndex, linePosition: "".startIndex, lineNumber: 0)
    }

    public func tokenize(_ string: String, onFound: Action) throws {
        try tokenize(string, onFound: onFound, onError: nil)
    }

    public func tokenize(_ string: String, onFound: Action, onError: ErrorAction?) throws {
        currentPosition = (tokenPosition: string.startIndex, linePosition: string.startIndex, lineNumber: 1)
        var errorStartPosition: CitronLexerPosition? = nil
        while (currentPosition.tokenPosition < string.endIndex) {
            var matched = false
            for rule in rules {
                switch (rule) {
                case .string(let ruleString, let tokenData):
                    if (string.suffix(from: currentPosition.tokenPosition).hasPrefix(ruleString)) {
                        if let errorStartPosition = errorStartPosition {
                            try onError?(CitronLexerError.noMatchingRuleAt(errorPosition: errorStartPosition))
                        }
                        if let tokenData = tokenData {
                            try onFound(tokenData)
                        }
                        currentPosition = lexerPosition(in: string, advancedFrom: currentPosition, by: ruleString.count)
                        errorStartPosition = nil
                        matched = true
                    }
                case .regex(let ruleRegex, let handler):
                    let result = ruleRegex.firstMatch(in: string, options: .anchored, range:
                        NSRange(
                            location: string.prefix(upTo: currentPosition.tokenPosition).utf16.count,
                            length: string.suffix(from: currentPosition.tokenPosition).utf16.count)
                    )
                    if let matchingRange = result?.range {
                        let start = string.utf16.index(string.utf16.startIndex, offsetBy: matchingRange.lowerBound)
                        let end = string.utf16.index(string.utf16.startIndex, offsetBy: matchingRange.upperBound)
                        if let matchingString = String(string.utf16[start..<end]) {
                            if let errorStartPosition = errorStartPosition {
                                try onError?(CitronLexerError.noMatchingRuleAt(errorPosition: errorStartPosition))
                            }
                            if let tokenData = handler(matchingString) {
                                try onFound(tokenData)
                            }
                            currentPosition = lexerPosition(in: string, advancedFrom: currentPosition, by: matchingString.count)
                            errorStartPosition = nil
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
                if (onError == nil) {
                    throw CitronLexerError.noMatchingRuleAt(errorPosition: currentPosition)
                } else {
                    if (errorStartPosition == nil) {
                        errorStartPosition = currentPosition
                    }
                    currentPosition = lexerPosition(in: string, advancedFrom: currentPosition, by: 1)
                }
            }
        }
        if let errorStartPosition = errorStartPosition {
            try onError?(CitronLexerError.noMatchingRuleAt(errorPosition: errorStartPosition))
        }
    }
}

public enum CitronLexerError: Error {
    case noMatchingRuleAt(errorPosition: CitronLexerPosition)
}

private extension CitronLexer {
    func lexerPosition(in str: String, advancedFrom from: CitronLexerPosition, by offset: Int) -> CitronLexerPosition {
         let tokenPosition = str.index(from.tokenPosition, offsetBy: offset)
         var linePosition = from.linePosition
         var lineNumber = from.lineNumber
         var index = from.tokenPosition
         while (index < tokenPosition) {
            if (str[index] == "\n") {
                lineNumber = lineNumber + 1
                linePosition = str.index(after: index)
            }
            index = str.index(after: index)
         }
         return (tokenPosition: tokenPosition, linePosition: linePosition, lineNumber: lineNumber)
    }
}
