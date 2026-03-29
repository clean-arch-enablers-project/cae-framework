package com.cae.framework.autofeatures.autoauth.scope_expression;

import com.cae.mapped_exceptions.specifics.InternalMappedException;

import java.util.ArrayList;
import java.util.List;

public class ScopeExpressionParser {

    private final String useCaseName;
    private final List<ScopeExpressionToken> tokens;
    private int index = 0;
    private int parenthesisDepth = 0;

    public ScopeExpressionParser(String expression, String useCaseName) {
        this.useCaseName = useCaseName;
        if (expression == null || expression.isBlank())
            throw new InternalMappedException(
                "Couldn't instantiate '" + useCaseName + "'",
                "Invalid scope expression. The expression is blank."
            );
        this.tokens = tokenize(expression);
    }

    public ScopeExpressionNode parse() {
        var node = parseOr();
        if (!isAtEnd())
            throw new InternalMappedException(
                "Couldn't instantiate '" + this.useCaseName + "'",
                "Invalid scope expression. Unexpected token '" + peek().value + "'."
            );
        return node;
    }

    private ScopeExpressionNode parseOr() {
        var node = parseAnd();
        while (match(ScopeExpressionToken.TokenType.OR)) {
            var right = parseAnd();
            node = new OrNode(node, right);
        }
        return node;
    }

    private ScopeExpressionNode parseAnd() {
        var node = parseUnary();
        while (match(ScopeExpressionToken.TokenType.AND)) {
            var right = parseUnary();
            node = new AndNode(node, right);
        }
        return node;
    }

    private ScopeExpressionNode parseUnary() {
        if (match(ScopeExpressionToken.TokenType.NOT))
            return new NotNode(parseUnary());
        return parsePrimary();
    }

    private ScopeExpressionNode parsePrimary() {
        if (match(ScopeExpressionToken.TokenType.OPEN_PAREN)) {
            if (this.parenthesisDepth > 0)
                throw new InternalMappedException(
                    "Couldn't instantiate '" + this.useCaseName + "'",
                    "Invalid scope expression. Nested parentheses are not allowed."
                );
            this.parenthesisDepth++;
            var node = parseOr();
            if (!match(ScopeExpressionToken.TokenType.CLOSE_PAREN))
                throw new InternalMappedException(
                    "Couldn't instantiate '" + this.useCaseName + "'",
                    "Invalid scope expression. Missing closing parenthesis."
                );
            this.parenthesisDepth--;
            return node;
        }
        if (match(ScopeExpressionToken.TokenType.SCOPE))
            return new ScopeNode(previous().value);
        if (isAtEnd())
            throw new InternalMappedException(
                "Couldn't instantiate '" + this.useCaseName + "'",
                "Invalid scope expression. Expression ended unexpectedly."
            );
        throw new InternalMappedException(
            "Couldn't instantiate '" + this.useCaseName + "'",
            "Invalid scope expression. Unexpected token '" + peek().value + "'."
        );
    }

    private boolean match(ScopeExpressionToken.TokenType type) {
        if (check(type)) {
            this.index++;
            return true;
        }
        return false;
    }

    private boolean check(ScopeExpressionToken.TokenType type) {
        if (isAtEnd())
            return false;
        return peek().type == type;
    }

    private boolean isAtEnd() {
        return this.index >= this.tokens.size();
    }

    private ScopeExpressionToken peek() {
        return this.tokens.get(this.index);
    }

    private ScopeExpressionToken previous() {
        return this.tokens.get(this.index - 1);
    }

    private List<ScopeExpressionToken> tokenize(String expression) {
        var tokens = new ArrayList<ScopeExpressionToken>();
        var cursor = 0;
        while (cursor < expression.length()) {
            var current = expression.charAt(cursor);
            if (Character.isWhitespace(current)) {
                cursor++;
                continue;
            }
            if (current == '(') {
                tokens.add(new ScopeExpressionToken(ScopeExpressionToken.TokenType.OPEN_PAREN, "("));
                cursor++;
                continue;
            }
            if (current == ')') {
                tokens.add(new ScopeExpressionToken(ScopeExpressionToken.TokenType.CLOSE_PAREN, ")"));
                cursor++;
                continue;
            }
            if (current == '!') {
                tokens.add(new ScopeExpressionToken(ScopeExpressionToken.TokenType.NOT, "!"));
                cursor++;
                continue;
            }
            if (current == '&') {
                if (cursor + 1 < expression.length() && expression.charAt(cursor + 1) == '&') {
                    tokens.add(new ScopeExpressionToken(ScopeExpressionToken.TokenType.AND, "&&"));
                    cursor = cursor + 2;
                    continue;
                }
                throw new InternalMappedException(
                    "Couldn't instantiate '" + this.useCaseName + "'",
                    "Invalid scope expression. Invalid operator '&'. Use '&&'."
                );
            }
            if (current == '|') {
                if (cursor + 1 < expression.length() && expression.charAt(cursor + 1) == '|') {
                    tokens.add(new ScopeExpressionToken(ScopeExpressionToken.TokenType.OR, "||"));
                    cursor = cursor + 2;
                    continue;
                }
                throw new InternalMappedException(
                    "Couldn't instantiate '" + this.useCaseName + "'",
                    "Invalid scope expression. Invalid operator '|'. Use '||'."
                );
            }
            var start = cursor;
            while (cursor < expression.length()) {
                var content = expression.charAt(cursor);
                if (Character.isWhitespace(content) || content == '(' || content == ')' || content == '&' || content == '|' || content == '!')
                    break;
                cursor++;
            }
            if (start == cursor)
                throw new InternalMappedException(
                    "Couldn't instantiate '" + this.useCaseName + "'",
                    "Invalid scope expression. Invalid token around '" + current + "'."
                );
            var scope = expression.substring(start, cursor);
            if (scope.isEmpty())
                throw new InternalMappedException(
                    "Couldn't instantiate '" + this.useCaseName + "'",
                    "Invalid scope expression. Invalid empty scope token."
                );
            tokens.add(new ScopeExpressionToken(ScopeExpressionToken.TokenType.SCOPE, scope));
        }
        if (tokens.isEmpty())
            throw new InternalMappedException(
                "Couldn't instantiate '" + this.useCaseName + "'",
                "Invalid scope expression. Expression has no tokens."
            );
        return tokens;
    }
}
