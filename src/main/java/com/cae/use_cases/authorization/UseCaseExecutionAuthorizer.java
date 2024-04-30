package com.cae.use_cases.authorization;

import com.cae.use_cases.correlations.actors.Actor;

public interface UseCaseExecutionAuthorizer {

    boolean allows(Actor actor, String[] requiredScopes);




}
