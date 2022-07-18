%class_name FunctionHeaderParser

%preface {
    import CitronParserModule
    import CitronLexerModule
    typealias TypeIdentifier = String
}

%token_type "(token: Token, position: CitronLexerPosition)"
%tokencode_prefix funcHeader

%nonterminal_type func_header "FunctionHeader?"
%nonterminal_type func_keyword_name "TypeIdentifier?"
%nonterminal_type func_signature "([FunctionParameter?]?, FunctionHeader.Throwability, TypeIdentifier)?"

func_header ::= func_keyword_name(n) func_signature(sig). {
    if let sig = sig {
        return FunctionHeader(name: n, parameters: sig.0,
            returnType: sig.2, throwability: sig.1)
    } else {
        return nil
    }
}

func_keyword_name ::= KeywordFunc Identifier(func_name). { return func_name.token.toIdentifierString()! }

func_signature ::= param_clause(p). { return (p, .nonthrowing, "Void") }
func_signature ::= param_clause(p) func_result(r). { return (p, .nonthrowing, r) }
func_signature ::= param_clause(p) throw_clause(t) func_result(r). { return (p, t, r) }
func_signature ::= param_clause(p) throw_clause(t). { return (p, t, "Void") }

%nonterminal_type throw_clause "FunctionHeader.Throwability"
%nonterminal_type func_result TypeIdentifier

throw_clause ::= KeywordThrows(t). { return .throwing }
throw_clause ::= KeywordRethrows(t). { return .rethrowing }
func_result ::= Arrow type(t). { return t }

%nonterminal_type param_clause "[FunctionParameter?]?"
%nonterminal_type param_list "[FunctionParameter?]"
%nonterminal_type param "FunctionParameter?"

param_clause ::= OpenBracket CloseBracket. { return [] }
param_clause ::= OpenBracket param_list(list) CloseBracket. { return list }
param_list ::= param(p). { return [p] }
param_list ::= param(p) Comma param_list(list). { return [p] + list }
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

external_param_name ::= Identifier(t). { return t.token.toIdentifierString()! }
local_param_name ::= Identifier(t). { return t.token.toIdentifierString()! }

%nonterminal_type type_annotation "(type: String, isInout: Bool)"
%nonterminal_type type TypeIdentifier

type_annotation ::= Colon type(t). { return (type: t, isInout: false) }
type_annotation ::= Colon KeywordInout type(t). { return (type: t, isInout: true) }
type ::= Identifier(t). { return t.token.toIdentifierString()! }

%capture_errors func_header.
%capture_errors func_signature.
%capture_errors func_keyword_name
    end_before(OpenBracket).
%capture_errors param_clause
    end_after(CloseBracket)
    end_before(KeywordThrows | KeywordRethrows | Arrow).
%capture_errors param
    end_before(Comma | CloseBracket)
    end_after([Colon, Identifier] | [KeywordInout, Identifier]).
