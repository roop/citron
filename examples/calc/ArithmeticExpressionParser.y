%class_name ArithmeticExpressionParser
%token_type Int
%default_nonterminal_type ArithmeticExpression

// (MULTIPLY, DIVIDE) have higher priority than (ADD, SUBTRACT)
// All four are defined as left associative.

%left_associative ADD SUBTRACT.
%left_associative MULTIPLY DIVIDE.

root ::= expr(a). {
    return a
}

expr ::= expr(a) ADD expr(b). {
    return .addition(a, b)
}

expr ::= expr(a) SUBTRACT expr(b). {
    return .subtraction(a, b)
}

expr ::= expr(a) MULTIPLY expr(b). {
    return .multiplication(a, b)
}

expr ::= expr(a) DIVIDE expr(b). {
    return .division(a, b)
}

expr ::= INTEGER(a). {
    return .number(a)
}
