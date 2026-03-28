package com.cae.framework.autofeatures.autoauth.scope_expression;

public class ScopeExpressionToken {

    public final TokenType type;
    public final String value;

    public ScopeExpressionToken(TokenType type, String value) {
        this.type = type;
        this.value = value;
    }

    public enum TokenType {
        AND, OR, NOT, OPEN_PAREN, CLOSE_PAREN, SCOPE
    }
}
