// swift-tools-version:5.3
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "citron",
    products: [
        .executable(
            name: "citron",
            targets: ["citron"]),
        .library(
            name: "CitronParserModule",
            targets: ["CitronParserModule"]),
        .library(
            name: "CitronLexerModule",
            targets: ["CitronLexerModule"]),
    ],
    targets: [
        .target(
            name: "citron"),
        .target(
            name: "CitronParserModule"),
        .target(
            name: "CitronLexerModule"),
    ]
)
