package com.cae.framework.autofeatures.autoauth.scope_expression;

import java.util.Set;

public interface ScopeExpressionNode {
    boolean evaluate(Set<String> providedScopes);
}
