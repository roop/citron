// swift-tools-version:5.6
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "citron",
    products: [
        .executable(name: "citron", targets: ["citron"]),
        .library(name: "CitronParserModule", targets: ["CitronParserModule"]),
        .library(name: "CitronLexerModule", targets: ["CitronLexerModule"]),
        .plugin(name: "CitronParserGenerator", targets: ["CitronParserGenerator"])
    ],
    targets: [
      .executableTarget(name: "citron"),
      .target(name: "CitronParserModule", exclude: ["LICENSE.txt"]),
      .target(name: "CitronLexerModule", exclude: ["LICENSE.txt"]),
      .plugin(
        name: "CitronParserGenerator", capability: .buildTool(),
        dependencies: ["citron"]),

      // Examples
      .executableTarget(
        name: "expr",
        dependencies: ["CitronParserModule", "CitronLexerModule"],
        path: "examples/expr",
        exclude: ["README.md", "Makefile"],
        plugins: [.plugin(name: "CitronParserGenerator")]),

      .executableTarget(
        name: "expr_ec",
        dependencies: ["CitronParserModule", "CitronLexerModule"],
        path: "examples/expr_ec",
        exclude: ["README.md", "Makefile"],
        plugins: [.plugin(name: "CitronParserGenerator")]),

      .executableTarget(
        name: "functype",
        dependencies: ["CitronParserModule", "CitronLexerModule"],
        path: "examples/functype",
        exclude: ["README.md", "Makefile"],
        plugins: [.plugin(name: "CitronParserGenerator")]),

      .executableTarget(
        name: "functype_ec",
        dependencies: ["CitronParserModule", "CitronLexerModule"],
        path: "examples/functype_ec",
        exclude: ["README.md", "Makefile"],
        plugins: [.plugin(name: "CitronParserGenerator")]),

      .testTarget(
          name: "CitronTests",
          dependencies: ["expr", "expr_ec", "functype", "functype_ec"]
      ),
    ]
)
