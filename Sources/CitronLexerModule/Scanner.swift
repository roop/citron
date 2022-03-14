// SPDX-License-Identifier: MIT
// Copyright (C) 2021 Dave Abrahams
// This file is part of the Citron Lexer Module

import Foundation

public struct Scanner<TokenID> {
  /// Creates an instance recognizing the given literal strings and patterns.
  ///
  /// - Parameter literalStrings: a mapping from literal strings to be
  ///   recognized to the corresponding token ID.
  /// - Parameter patterns: a mapping from regular expression pattern to either
  ///   a coresponding token ID, or `nil` if the pattern is to be discarded
  ///   (e.g. for whitespace).
  public init(
    literalStrings: [String: TokenID],
    patterns: [String: TokenID?]
  ) {
    /// A single regex pattern with alternatives for all the literals
    let literalPattern = literalStrings.keys
    // Put the longest ones first because regexps match alternatives eagerly.
      .sorted { $0.count > $1.count }
      .lazy.map { NSRegularExpression.escapedPattern(for: $0) }
      .joined(separator: "|")

    /// The literals pattern, followed by the user-specified patterns.
    ///
    /// The literals pattern is given the `nil` token ID, but the first element
    /// in this list is treated specially.
    let allPatterns = [(literalPattern, nil)] + patterns

    /// Compile to regexp matchers.
    self.matchers = allPatterns.map {
      try! (matcher: NSRegularExpression(pattern: $0, options: []), tokenID: $1)
    }

    self.literalStrings = literalStrings
  }

  /// A regexp paired with the corresponding tokenID, or `nil` to indicate that
  /// the pattern is to be skipped or that it indicates a literal string.
  fileprivate typealias Matcher
    = (matcher: NSRegularExpression, tokenID: TokenID?)

  /// The regexps matched by this scanner, paired with the corresponding
  /// tokenID.
  ///
  /// Except for the first entry, a `nil` `tokenID` indicates the pattern is to
  /// be skipped.  The first entry is special and indicates a literal string was
  /// matched.
  private let matchers: [Matcher]

  /// A mapping from literal strings to be recognized to the corresponding token
  /// ID.
  private let literalStrings: [String: TokenID]
}

public extension Scanner {
  /// The sequence of tokens in a source file.
  ///
  /// An .ILLEGAL_CHARACTER token is produced for each character that isn't
  /// otherwise recognized.
  struct Tokens {
    /// The complete text from which `self`'s tokens will be derived.
    private let sourceText: String
    /// The name of the file embedded in each token's source region.
    private let sourceFileName: String

    /// The regexps to be matched, paired with the corresponding tokenID.
    ///
    /// Except for the first entry, a `nil` `tokenID` indicates the pattern is
    /// to be skipped.  The first entry is special and indicates a literal
    /// string was matched.
    private let matchers: [Matcher]

    /// A mapping from literal strings to be recognized to the corresponding
    /// token ID.
    private let literalStrings: [String: TokenID]

    /// A tokenID used when no pattern matches in the source text.
    private let unrecognizedToken: TokenID
  }

  /// Returns the sequence of tokens in the given source text, with their
  /// source ranges indicating they were extracted from the file at `path`.
  func tokens(
    in sourceText: String, fromFile path: String,
    unrecognizedToken: TokenID
  ) -> Tokens {
    return .init(
      in: sourceText, fromFile: path,
      matchers: matchers, literalStrings: literalStrings,
      unrecognizedToken: unrecognizedToken)
  }
}

/// The sequence of tokens in a source file.
///
/// An .ILLEGAL_CHARACTER token is produced for each character that isn't
/// otherwise recognized.
extension Scanner.Tokens: Sequence {
  /// Creates an instance that extracts the tokens from `sourceText`, labeling
  /// each as having come from the given source file.
  fileprivate init(
    in sourceText: String, fromFile sourceFileName: String,
    matchers: [Scanner.Matcher], literalStrings: [String: TokenID],
    unrecognizedToken: TokenID
  ) {
    self.sourceText = sourceText
    self.sourceFileName = sourceFileName
    self.matchers = matchers
    self.literalStrings = literalStrings
    self.unrecognizedToken = unrecognizedToken
  }

  /// Returns a new iteration state.
  public func makeIterator() -> Iterator {
    .init(
      over: sourceText, from: sourceFileName,
      matchers: matchers, literalStrings: literalStrings,
      unrecognizedToken: unrecognizedToken)
  }

  /// The token stream's iteration state and element producer.
  public struct Iterator: IteratorProtocol {
    /// Creates an instance producing the tokens from `sourceText`, labeling
    /// each as having come from the given source file.
    fileprivate init(
      over sourceText: String, from sourceFileName: String,
      matchers: [Scanner.Matcher], literalStrings: [String: TokenID],
      unrecognizedToken: TokenID)
    {
      self.sourceText = sourceText
      textPosition = sourceText.startIndex
      sourceUTF16Length = sourceText.utf16.count
      self.sourceFileName = sourceFileName
      self.matchers = matchers
      self.literalStrings = literalStrings
      self.unrecognizedToken = unrecognizedToken
    }

    public typealias Element = (TokenID, Substring, SourceRegion)

    /// Returns the next token in the source, or `nil` if the source is
    /// exhausted.
    public mutating func next() -> Element? {
      // Repeat until a non-ignored pattern is matched.
      while utf16Offset < sourceUTF16Length {
        // NSRegularExpression matching region
        let remainingUTF16 = NSRange(
          location: utf16Offset, length: sourceUTF16Length - utf16Offset)

        // UTF16 lengths matched by each matcher
        let matchUTF16Lengths = matchers.lazy.map { [sourceText] in
          $0.matcher.firstMatch(
            in: sourceText, options: .anchored, range: remainingUTF16
          )?.range.length ?? 0
        }

        // Choose the longest matcher.
        let (bestMatchIndex, bestMatchUTF16Length)
          = matchUTF16Lengths.enumerated().max(by: { $0.element < $1.element })!

        let tokenStart = textPosition
        let remainingText = sourceText[tokenStart...]

        // Advance past the recognized text, or the first character if nothing
        // matched.
        textPosition = bestMatchUTF16Length == 0
          ? remainingText.dropFirst(1).startIndex
          : remainingText.utf16.dropFirst(bestMatchUTF16Length).startIndex

        let tokenText = remainingText[..<textPosition]
        utf16Offset += tokenText.utf16.count

        let tokenRegionStart = sourcePosition

        // Adjust human-readable source position
        let tokenLines = tokenText.split(
          omittingEmptySubsequences: false, whereSeparator: \.isNewline)
        let newlineCount = tokenLines.count - 1
        sourcePosition.line += newlineCount
        sourcePosition.column
          = (newlineCount == 0 ? sourcePosition.column : 1)
          + tokenLines.last!.count

        if let matchedID = bestMatchUTF16Length == 0 ? unrecognizedToken
             : bestMatchIndex == 0 ? literalStrings[String(tokenText)]
             : matchers[bestMatchIndex].tokenID
        {
          return (
            matchedID, tokenText,
            SourceRegion(
              fileName: sourceFileName, tokenRegionStart..<sourcePosition))
        }
      }
      return nil
    }

    /// The complete text being matched
    private let sourceText: String
    /// The name of the file embedded in each token's source region.
    private let sourceFileName: String
    /// The number of UTF-16 code units in `sourceText`.
    private let sourceUTF16Length: Int

    /// Where scanning for the next token will resume (human-readable form).
    private var sourcePosition = SourcePosition.start
    /// Where scanning for the next token will resume (string form).
    private var textPosition: String.Index
    /// Where scanning for the next token will resume (NSRegularExpression form).
    private var utf16Offset: Int = 0

    /// The regexps matched by this scanner, paired with the corresponding
    /// tokenID.
    ///
    /// Except for the first entry, a `nil` `tokenID` indicates the pattern is to
    /// be skipped.  The first entry is special and indicates a literal string was
    /// matched.
    private let matchers: [Scanner.Matcher]

    /// A mapping from literal strings to be recognized to the corresponding
    /// token ID.
    private let literalStrings: [String: TokenID]

    /// A tokenID used when no pattern matches in the source text.
    private let unrecognizedToken: TokenID
  }
}


extension String {
  /// Accesses the slice of `self` specified by the given range of UTF16
  /// offsets.
  fileprivate subscript(r: NSRange) -> Substring {
    let start = utf16.index(
      startIndex, offsetBy: r.location, limitedBy: endIndex) ?? endIndex
    let end = utf16.index(start, offsetBy: r.length, limitedBy: endIndex)
      ?? endIndex
    return self[start..<end]
  }
}
