
GRAMMAR = ArithmeticExpressionParser
NAME = expr
CC = clang
SWIFTC = swiftc
BIN = ../../bin
SRC = ../../Sources
CITRON = ${BIN}/citron

build: ${NAME}

run: ${NAME}
	./${NAME} "1 + 2 * 3 - 4"

clean:
	rm -rf ./${NAME} ${GRAMMAR}.swift ${CITRON}

${CITRON}: ${SRC}/citron/main.c
	mkdir -p ${BIN} && ${CC} $^ -o $@

${GRAMMAR}.swift: ${CITRON} ${GRAMMAR}.y
	${CITRON} ${GRAMMAR}.y -o $@

${NAME}: ${SRC}/CitronParserModule/CitronParser.swift ${SRC}/CitronLexerModule/CitronLexer.swift ${GRAMMAR}.swift main.swift
	${SWIFTC} $^ -o $@
