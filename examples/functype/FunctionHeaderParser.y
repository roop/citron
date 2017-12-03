%class_name FunctionHeaderParser

%preface {
    enum Token {
        case keyword // for FUNC, THROWS, INOUT, etc.
        case punctuation // for (, ), ->, etc.
        case identifier(String) // for IDENTIFIER
        func toIdentifierString() -> String? {
            if case .identifier(let id) = self {
                return id
            }
            return nil
        }
    }
    typealias TypeIdentifier = String
}

%token_type Token

%nonterminal_type func_header FunctionHeader
// FunctionHeader is defined in main.swift
%nonterminal_type func_name TypeIdentifier
%nonterminal_type func_signature "([FunctionParameter], FunctionHeader.Throwability, TypeIdentifier)"

func_header ::= FUNC func_name(n) func_signature(sig). {
    return FunctionHeader(name: n, parameters: sig.0,
        returnType: sig.2, throwability: sig.1)
}

func_name ::= IDENTIFIER(token). { return token.toIdentifierString()! }

func_signature ::= param_clause(p). { return (p, .nonthrowing, "Void") }
func_signature ::= param_clause(p) func_result(r). { return (p, .nonthrowing, r) }
func_signature ::= param_clause(p) throw_clause(t) func_result(r). { return (p, t, r) }
func_signature ::= param_clause(p) throw_clause(t). { return (p, t, "Void") }

%nonterminal_type throw_clause "FunctionHeader.Throwability"
// FunctionHeader.Throwability is defined in main.swift
%nonterminal_type func_result TypeIdentifier

throw_clause ::= THROWS(t). { return .throwing }
throw_clause ::= RETHROWS(t). { return .rethrowing }
func_result ::= FUNC_ARROW type(t). { return t }

%nonterminal_type param_clause "[FunctionParameter]"
%nonterminal_type param_list "[FunctionParameter]"
%nonterminal_type param FunctionParameter
// FunctionParameter is defined in main.swift

param_clause ::= L_BR R_BR. { return [] }
param_clause ::= L_BR param_list(list) R_BR. { return list }
param_list ::= param(p). { return [p] }
param_list ::= param(p) COMMA param_list(list). { return [p] + list }
param ::= local_param_name(lpn) type_annotation(ta). {
    return FunctionParameter(localName: lpn,
            externalName: nil, type: ta.type, isInout: ta.isInout)
}
param ::= external_param_name(epn) local_param_name(lpn) type_annotation(ta). {
    return FunctionParameter(localName: lpn,
            externalName: epn, type: ta.type, isInout: ta.isInout)
}

%nonterminal_type external_param_name String
%nonterminal_type local_param_name String

external_param_name ::= IDENTIFIER(token). { return token.toIdentifierString()! }
local_param_name ::= IDENTIFIER(token). { return token.toIdentifierString()! }

%nonterminal_type type_annotation "(type: String, isInout: Bool)"
%nonterminal_type type TypeIdentifier

type_annotation ::= COLON type(t). { return (type: t, isInout: false) }
type_annotation ::= COLON INOUT type(t). { return (type: t, isInout: true) }
type ::= IDENTIFIER(token). { return token.toIdentifierString()! }
