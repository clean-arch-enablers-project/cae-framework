package com.cae.framework.autofeatures.autoauth.scope_expression;

import java.util.Set;

public class ScopeNode implements ScopeExpressionNode {

    private final String scope;

    public ScopeNode(String scope) {
        this.scope = scope;
    }

    @Override
    public boolean evaluate(Set<String> providedScopes) {
        return providedScopes.contains(this.scope);
    }
}
