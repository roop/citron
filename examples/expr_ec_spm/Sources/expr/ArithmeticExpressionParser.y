// Name of the output Swift class

%class_name ArithmeticExpressionParser

// Import

%preface {import CitronParserModule}

// Type for terminals

%token_type Int


// Type for non-terminals

%nonterminal_type root ArithmeticExpression
%nonterminal_type expr ArithmeticExpression

// The ArithmeticExpression type is defined in main.swift


// Associativity and precedences

%left_associative Add Subtract.
%left_associative Multiply Divide.

// (Multiply, Divide) have higher precedence than (Add, Subtract)
// All four are defined as left associative.


// Grammar rules

root ::= expr(a). {
    return a
}

expr ::= expr(a) Add expr(b). {
    return .addition(a, b)
}

expr ::= expr(a) Subtract expr(b). {
    return .subtraction(a, b)
}

expr ::= expr(a) Multiply expr(b). {
    return .multiplication(a, b)
}

expr ::= expr(a) Divide expr(b). {
    return .division(a, b)
}

expr ::= OpenBracket expr(a) CloseBracket. {
    return a
}

expr ::= Integer(a). {
    return .number(a)
}

%capture_errors root.
%capture_errors expr
    end_before(CloseBracket | Add | Subtract | Multiply | Divide).
