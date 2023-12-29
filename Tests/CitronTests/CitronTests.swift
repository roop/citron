import XCTest
@testable import expr
@testable import expr_ec
@testable import functype
@testable import functype_ec

final class CitronTests: XCTestCase {

  func test_expr() throws {
    let (lexer, parser) = expr.makeLexerAndParser()
    try lexer.tokenize("1 + 2 * 3") { (t, c) in
      try parser.consume(token: t, code: c)
    }
    let tree = try parser.endParsing()
    XCTAssertEqual("\(tree)", "(+ 1 (* 2 3))")
  }

  func test_expr_ec() throws {
    let input = "1 + 2 * 3"
    let (lexer, parser) = expr_ec.makeLexerAndParser(input: input)

    try lexer.tokenize(
      input,
      onFound: { t in
        try parser.consume(token: t.token, code: t.code)
      },
      onError: { e in
        try parser.consume(lexerError: e)
      })
    let tree = try parser.endParsing()
    XCTAssertEqual("\(tree)", "(+ 1 (* 2 3))")
  }

  func test_functype() {
    if let funcHeader = functype.parseFunctionHeader(input: "func add(a: Int, b: Int) -> Int") {
      XCTAssertEqual(funcHeader.typeString(), "(Int, Int) -> Int")
    }
    else { XCTFail("unexpected parse failure") }
  }

  func test_functype_ec() {
    if let funcHeader = functype_ec.parseFunctionHeader(input: "func add(a: Int, b: Int) -> Int") {
      XCTAssertEqual(funcHeader.typeString(), "(Int, Int) -> Int")
    }
    else { XCTFail("unexpected parse failure") }
  }

}
