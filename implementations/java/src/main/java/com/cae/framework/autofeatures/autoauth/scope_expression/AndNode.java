package com.cae.framework.autofeatures.autoauth.scope_expression;

import java.util.Set;

public class AndNode implements ScopeExpressionNode {

    private final ScopeExpressionNode left;
    private final ScopeExpressionNode right;

    public AndNode(ScopeExpressionNode left, ScopeExpressionNode right) {
        this.left = left;
        this.right = right;
    }

    @Override
    public boolean evaluate(Set<String> providedScopes) {
        return this.left.evaluate(providedScopes) && this.right.evaluate(providedScopes);
    }
}
