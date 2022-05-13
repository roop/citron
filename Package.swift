// swift-tools-version:5.6
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
        .executableTarget(
            name: "citron"),
        .target(
            name: "CitronParserModule",
            exclude: ["LICENSE.txt"]),
        .target(
            name: "CitronLexerModule",
            exclude: ["LICENSE.txt"]),
    ]
)
