package com.cae.framework.autofeatures.autoauth.scope_expression;

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
        var parser = new ScopeExpressionParser(expression, name);
        return new ScopeExpression(expression, parser.parse());
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
}
