package com.cae.autofeatures.autoauth;

import com.cae.use_cases.contexts.actors.Actor;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.stream.Stream;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ScopeBasedAuth {

    public static boolean allows(Actor actor, String[] requiredScopes){
        var providedScopes = actor.getScopes();
        return Stream.of(requiredScopes)
                .allMatch(requiredScope -> checkAllRequiredScopes(requiredScope, providedScopes));
    }

    private static boolean checkAllRequiredScopes(String requiredScope, List<String> providedScopes) {
        var options = getOptionsOutta(requiredScope);
       return providedScopes.stream()
               .anyMatch(providedScope -> checkAnyOption(providedScope, options));
    }

    private static List<String> getOptionsOutta(String requiredScope) {
        return List.of(requiredScope.split("\\|\\|"));
    }

    private static boolean checkAnyOption(String providedScope, List<String> options) {
        return options.stream()
                .anyMatch(option -> option.trim().equals(providedScope));
    }

}


