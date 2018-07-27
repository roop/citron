

extension FunctionHeaderParser : FunctionHeaderParser.CitronErrorCaptureDelegate {
    func shouldCaptureErrorOnParam(error: FunctionHeaderParser.UnexpectedTokenError,
        resolvedSymbols: [(name: String, value: Any)],
        unclaimedTokens: [(token: FunctionHeaderParser.CitronToken, tokenCode: FunctionHeaderParser.CitronTokenCode)],
        nextToken: (token: FunctionHeaderParser.CitronToken, tokenCode: FunctionHeaderParser.CitronTokenCode)?) -> CitronErrorCaptureResponse<FunctionParameter?> {
        reportErrorOnParam(resolvedSymbols: resolvedSymbols, unclaimedTokens: unclaimedTokens, nextToken: nextToken)
        return .captureAs(nil)
    }

}

extension FunctionHeaderParser {
    func reportErrorOnParam(
        resolvedSymbols: [(name: String, value: Any)],
        unclaimedTokens: [(token: FunctionHeaderParser.CitronToken, tokenCode: FunctionHeaderParser.CitronTokenCode)],
        nextToken: (token: FunctionHeaderParser.CitronToken, tokenCode: FunctionHeaderParser.CitronTokenCode)?) {

        let firstUnresolvedToken = unclaimedTokens.first ?? nextToken
        let tokenStr = tokenString(tokenData: firstUnresolvedToken)
        if let lastResolvedSymbolName = resolvedSymbols.last?.name {
            switch (lastResolvedSymbolName) {
            case "local_param_name": fallthrough
            case "external_param_name":
                croak(expected: "':' after parameter name", butGot: tokenStr)
            case "Colon":
                croak(expected: "type identifier or 'inout' after ':'", butGot: tokenStr)
            case "KeywordInout":
                croak(expected: "type identifier after 'inout'", butGot: tokenStr)
            default:
                croak(expected: "',' or ')' after parameter specification", butGot: tokenStr)
            }
        }

    }

    func tokenString(tokenData: (token: FunctionHeaderParser.CitronToken, tokenCode: FunctionHeaderParser.CitronTokenCode)?) -> String {
        if let tokenData = tokenData {
            switch (tokenData.token) {
            case .keyword:
                switch (tokenData.tokenCode) {
                case .funcHeaderKeywordFunc: return "func"
                case .funcHeaderKeywordThrows: return "throws"
                case .funcHeaderKeywordRethrows: return "rethrows"
                case .funcHeaderKeywordInout: return "inout"
                default: return ""
                }
            case .punctuation:
                switch (tokenData.tokenCode) {
                case .funcHeaderArrow: return "->"
                case .funcHeaderOpenBracket: return "("
                case .funcHeaderCloseBracket: return ")"
                case .funcHeaderComma: return ","
                case .funcHeaderColon: return ":"
                default: return ""
                }
            case .identifier(let string):
                return string
            }
        } else {
            return "EOF"
        }
    }

    func croak(expected message: String, butGot actual: String) {
        print("Error: Expected \(message), but got \'\(actual)\'.")
    }
}
