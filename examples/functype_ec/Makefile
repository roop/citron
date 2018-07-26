
GRAMMAR = FunctionHeaderParser
NAME = functype
CC = clang
SWIFTC = swiftc
BIN = ../../bin
SRC = ../../src
CITRON = ${BIN}/citron

build: ${NAME}

run: ${NAME}
	./${NAME} "func add(a: Int, b: Int) -> Int"

clean:
	rm -rf ./${NAME} ${GRAMMAR}.swift ${CITRON}

${CITRON}: ${SRC}/citron.c
	mkdir -p ${BIN} && ${CC} $^ -o $@

${GRAMMAR}.swift: ${CITRON} ${GRAMMAR}.y
	${CITRON} ${GRAMMAR}.y -o $@

${NAME}: ${SRC}/CitronParser.swift ${SRC}/CitronLexer.swift ${GRAMMAR}.swift main.swift
	${SWIFTC} $^ -o $@
