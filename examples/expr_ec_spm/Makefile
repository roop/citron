# This Makefile invokes Swift Package Manager commands to build

GRAMMAR = Sources/expr/ArithmeticExpressionParser

build: ${GRAMMAR}.swift
	swift build --target expr

${GRAMMAR}.swift: ${GRAMMAR}.y
	rm -f $@
	swift build --target citron # Build citron executable
	swift run citron ${GRAMMAR}.y -o $@ # Generate the grammar

clean:
	rm -rf ${GRAMMAR}.swift .build

run: build
	swift run expr "1 + 2 * 3 - 4"
