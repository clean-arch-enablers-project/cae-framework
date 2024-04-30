package com.cae.use_cases.authorization;

import com.cae.use_cases.correlations.actors.Actor;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.util.stream.Stream;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class UseCaseExecutionAuthorizer {

    public static boolean allows(Actor actor, String[] requiredScopes){
        var providedScopes = actor.getScopes();
        return Stream.of(requiredScopes).allMatch(requiredScope -> providedScopes.stream().anyMatch(providedScope -> providedScope.equals(requiredScope)));
    }

}


