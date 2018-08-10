// Name of the output Swift class

%class_name ArithmeticExpressionParser


// Type for terminals

%token_type Int


// Type for non-terminals

%nonterminal_type root ArithmeticExpression
%nonterminal_type expr ArithmeticExpression

// The ArithmeticExpression type is defined in main.swift


// Associativity and precedences

%left_associative ADD SUBTRACT.
%left_associative MULTIPLY DIVIDE.

// (MULTIPLY, DIVIDE) have higher precedence than (ADD, SUBTRACT)
// All four are defined as left associative.


// Grammar rules

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

expr ::= OPEN_BR expr(a) CLOSE_BR. {
    return a
}

expr ::= INTEGER(a). {
    return .number(a)
}

%capture_errors root.
%capture_errors expr
    end_before(CLOSE_BR | ADD | SUBTRACT | MULTIPLY | DIVIDE).
