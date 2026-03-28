package com.cae.framework.autofeatures.autoauth;

import com.cae.framework.autofeatures.autoauth.scope_expression.ScopeExpressionNode;
import com.cae.framework.autofeatures.autoauth.scope_expression.ScopeExpressionParser;
import com.cae.mapped_exceptions.specifics.InternalMappedException;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public final class ScopeExpression {

    private final String rawExpression;
    private final ScopeExpressionNode root;

    private ScopeExpression(String rawExpression, ScopeExpressionNode root) {
        this.rawExpression = rawExpression;
        this.root = root;
    }

    public static ScopeExpression compile(String expression, String useCaseName) {
        var name = useCaseName != null ? useCaseName : "unknown";
        if (expression == null || expression.isBlank())
            throw invalidExpression(name, "The expression is blank.");
        try {
            var parser = new ScopeExpressionParser(expression);
            return new ScopeExpression(expression, parser.parse());
        } catch (InternalMappedException e) {
            throw e;
        } catch (RuntimeException e) {
            throw invalidExpression(name, "Unexpected parser error: " + e.getMessage());
        }
    }

    public String getRawExpression() {
        return this.rawExpression;
    }

    public boolean evaluate(List<String> providedScopes) {
        Set<String> provided = new HashSet<>();
        if (providedScopes != null) {
            for (String scope : providedScopes) {
                if (scope != null)
                    provided.add(scope);
            }
        }
        return this.root.evaluate(provided);
    }

    private static InternalMappedException invalidExpression(String useCaseName, String details) {
        return new InternalMappedException(
            "Couldn't instantiate '" + useCaseName + "'",
            "Invalid scope expression. " + details
        );
    }
}
