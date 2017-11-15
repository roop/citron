
GRAMMAR = ArithmeticExpressionParser
NAME = calc
CC = clang
SWIFTC = swiftc
BIN = ../../bin
SRC = ../../src
CITRON = ${BIN}/citron

build: ${NAME}

run: ${NAME}
	./${NAME}

clean:
	rm -rf ./${NAME} ${GRAMMAR}.swift ${CITRON}

${CITRON}: ${SRC}/citron.c
	mkdir -p ${BIN} && ${CC} -o $@ $^

${GRAMMAR}.swift: ${CITRON} ${GRAMMAR}.y
	${CITRON} -o $@ ${GRAMMAR}.y

${NAME}: ${SRC}/CitronParser.swift ${GRAMMAR}.swift main.swift
	${SWIFTC} $^ -o $@
