// SPDX-License-Identifier: MIT
// Copyright (C) 2021 Dave Abrahams
// This file is part of the Citron Lexer Module

/// A position relative to the beginning of a source file, in terms understood
/// by text editors.
public struct SourcePosition: Comparable, Hashable {
  /// The 1-based line number of the position.
  public var line: Int
  /// The 1-based column number of the position.
  public var column: Int

  /// The first position in any file.
  public static let start = Self(line: 1, column: 1)

  /// Returns `true` iff `l` precedes `r`.
  public static func < (l: Self, r: Self) -> Bool {
    (l.line, l.column) < (r.line, r.column)
  }
}

/// A contiguous region of text in a particular source file.
public struct SourceRegion: Hashable {
  /// Creates an instance that covers `span` in the file named by `f`.
  init(fileName f: String, _ span: Range<SourcePosition>) {
    self.fileName = f
    self.span = span
  }

  /// The name of the file within which this region resides.
  public let fileName: String

  /// The range of positions indicated by `self` in the file named by
  /// `fileName`.
  public let span: Range<SourcePosition>

  /// An empty location instance that can be used for synthesized AST nodes,
  /// etc.
  public static var empty
    = SourceRegion(fileName: "", .start ..< .start)

  /// Returns the region from the beginning of `first` to the end of `last`,
  /// unless one of `first` or `last` is empty, in which case the other one is
  /// returned.
  ///
  /// - Requires first or last is empty, or `site.fileName ==
  ///   last.fileName && first.span.lowerBound < last.span.upperBound`.
  public static func ... (first: Self, last: Self) -> Self
  {
    if first.span.isEmpty { return last }
    if last.span.isEmpty { return first }

    precondition(first.fileName == last.fileName)
    return Self(
      fileName: first.fileName, first.span.lowerBound..<last.span.upperBound)
  }
}

extension SourcePosition: CustomStringConvertible {
  /// A textual representation of `self`.
  public var description: String { "\(line).\(column)" }
}

extension SourceRegion: CustomStringConvertible, CustomDebugStringConvertible {
  /// A textual representation of `self` that is commonly recognized by IDEs
  /// when it shows up at the beginning of a diagnostic.
  public var description: String {
    "\(fileName):\(span.lowerBound)"
    + "-\(span.upperBound.line).\(span.upperBound.column - 1)"
  }

  /// A textual representation of `self` suitable for debugging.
  public var debugDescription: String {
    "SourceRegion(fileName: \(String(reflecting: fileName)), \(span))"
  }
}
