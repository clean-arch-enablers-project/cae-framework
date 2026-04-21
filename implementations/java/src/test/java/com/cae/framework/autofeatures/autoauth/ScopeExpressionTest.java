package com.cae.framework.autofeatures.autoauth;

import com.cae.framework.autofeatures.autoauth.scope_expression.ScopeExpression;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.List;

class ScopeExpressionTest {

    @Test
    @DisplayName("Should evaluate a simple scope expression")
    void shouldEvaluateSimpleScopeExpression() {
        var expression = ScopeExpression.compile("ADMIN", "SomeUseCase");
        Assertions.assertTrue(expression.evaluate(List.of("ADMIN")));
        Assertions.assertFalse(expression.evaluate(List.of("USER")));
    }

    @Test
    @DisplayName("Should compare scopes case-sensitively")
    void shouldCompareScopesCaseSensitively() {
        var expression = ScopeExpression.compile("admin && user", "SomeUseCase");
        Assertions.assertTrue(expression.evaluate(List.of("admin", "user")));
        Assertions.assertFalse(expression.evaluate(List.of("ADMIN", "USER")));
        Assertions.assertFalse(expression.evaluate(List.of("Admin", "user")));
    }

    @Test
    @DisplayName("Should preserve special characters in scope names")
    void shouldPreserveSpecialCharactersInScopeNames() {
        var expression = ScopeExpression.compile("app.read:write && user@tenant", "SomeUseCase");
        Assertions.assertTrue(expression.evaluate(List.of("app.read:write", "user@tenant")));
        Assertions.assertFalse(expression.evaluate(List.of("app.read:write")));
        Assertions.assertFalse(expression.evaluate(List.of("app.read-write", "user@tenant")));
    }

    @Test
    @DisplayName("Should evaluate OR expression")
    void shouldEvaluateOrExpression() {
        var expression = ScopeExpression.compile("ADMIN || MANAGER", "SomeUseCase");
        Assertions.assertTrue(expression.evaluate(List.of("ADMIN")));
        Assertions.assertTrue(expression.evaluate(List.of("MANAGER")));
        Assertions.assertFalse(expression.evaluate(List.of("USER")));
    }

    @Test
    @DisplayName("Should evaluate AND expression")
    void shouldEvaluateAndExpression() {
        var expression = ScopeExpression.compile("ADMIN && USER", "SomeUseCase");
        Assertions.assertTrue(expression.evaluate(List.of("ADMIN", "USER")));
        Assertions.assertFalse(expression.evaluate(List.of("ADMIN")));
    }

    @Test
    @DisplayName("Should evaluate nested expression")
    void shouldEvaluateNestedExpression() {
        var expression = ScopeExpression.compile("(ADMIN || MANAGER) && (USER || VISIT)", "SomeUseCase");
        Assertions.assertTrue(expression.evaluate(List.of("ADMIN", "VISIT")));
        Assertions.assertTrue(expression.evaluate(List.of("MANAGER", "USER")));
        Assertions.assertFalse(expression.evaluate(List.of("ADMIN")));
    }

    @Test
    @DisplayName("Should evaluate NOT expression")
    void shouldEvaluateNotExpression() {
        var expression = ScopeExpression.compile("!ADMIN", "SomeUseCase");
        Assertions.assertTrue(expression.evaluate(List.of("USER")));
        Assertions.assertFalse(expression.evaluate(List.of("ADMIN")));
    }

    @Test
    @DisplayName("Should evaluate expression with NOT and AND")
    void shouldEvaluateExpressionWithNotAndAnd() {
        var expression = ScopeExpression.compile("ADMIN && !USER", "SomeUseCase");
        Assertions.assertTrue(expression.evaluate(List.of("ADMIN")));
        Assertions.assertFalse(expression.evaluate(List.of("ADMIN", "USER")));
    }

    @Test
    @DisplayName("Should evaluate expression with parenthesis and NOT")
    void shouldEvaluateExpressionWithParenthesisAndNot() {
        var expression = ScopeExpression.compile("(ADMIN || MANAGER) && !BLOCKED", "SomeUseCase");
        Assertions.assertTrue(expression.evaluate(List.of("ADMIN")));
        Assertions.assertFalse(expression.evaluate(List.of("ADMIN", "BLOCKED")));
    }

    @Test
    @DisplayName("Should evaluate operator precedence with NOT over AND and OR")
    void shouldEvaluateOperatorPrecedenceWithNotOverAndAndOr() {
        var expression = ScopeExpression.compile("!ADMIN && USER || MANAGER", "SomeUseCase");
        Assertions.assertTrue(expression.evaluate(List.of("USER")));
        Assertions.assertTrue(expression.evaluate(List.of("MANAGER")));
        Assertions.assertFalse(expression.evaluate(List.of("ADMIN", "USER")));
    }

    @Test
    @DisplayName("Should reject invalid operator")
    void shouldRejectInvalidOperator() {
        Assertions.assertThrows(InternalMappedException.class, () -> ScopeExpression.compile("ADMIN & USER", "SomeUseCase"));
    }

    @Test
    @DisplayName("Should reject malformed parenthesis")
    void shouldRejectMalformedParenthesis() {
        Assertions.assertThrows(InternalMappedException.class, () -> ScopeExpression.compile("(ADMIN || USER", "SomeUseCase"));
    }

    @Test
    @DisplayName("Should handle null providedScopes as empty")
    void shouldHandleNullProvidedScopesAsEmpty() {
        var expression = ScopeExpression.compile("ADMIN", "SomeUseCase");
        Assertions.assertFalse(expression.evaluate(null));
    }

    @Test
    @DisplayName("Should reject nested parenthesis")
    void shouldRejectNestedParenthesis() {
        Assertions.assertThrows(InternalMappedException.class, () -> ScopeExpression.compile("((ADMIN))", "SomeUseCase"));
        Assertions.assertThrows(InternalMappedException.class, () -> ScopeExpression.compile("(ADMIN && (USER || MANAGER))", "SomeUseCase"));
        Assertions.assertThrows(InternalMappedException.class, () -> ScopeExpression.compile("(((ADMIN)))", "SomeUseCase"));
    }
}
