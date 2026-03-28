package com.cae.framework.autofeatures.autoauth.scope_expression;

import java.util.Set;

public class NotNode implements ScopeExpressionNode {

    private final ScopeExpressionNode target;

    public NotNode(ScopeExpressionNode target) {
        this.target = target;
    }

    @Override
    public boolean evaluate(Set<String> providedScopes) {
        return !this.target.evaluate(providedScopes);
    }
}
