
GRAMMAR = FunctionHeaderParser
NAME = functype
CC = clang
SWIFTC = swiftc
BIN = ../../bin
SRC = ../../Sources
CITRON = ${BIN}/citron

build: ${NAME}

run: ${NAME}
	./${NAME} "func add(a: Int, b: Int) -> Int"

clean:
	rm -rf ./${NAME} ${GRAMMAR}.swift ${CITRON}

${CITRON}: ${SRC}/citron/main.c
	mkdir -p ${BIN} && ${CC} $^ -o $@

${GRAMMAR}.swift: ${CITRON} ${GRAMMAR}.y
	${CITRON} ${GRAMMAR}.y -o $@

${NAME}: ${SRC}/CitronParserModule/CitronParser.swift ${SRC}/CitronLexerModule/CitronLexer.swift ${GRAMMAR}.swift ErrorReporter.swift main.swift
	${SWIFTC} -g $^ -o $@
