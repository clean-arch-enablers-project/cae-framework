package com.cae.framework.autofeatures.autoauth;

import com.cae.context.ExecutionContext;
import com.cae.context.actors.Actor;
import com.cae.framework.autofeatures.autoauth.exceptions.NotAllowedMappedException;
import com.cae.framework.use_cases.UseCase;
import com.cae.mapped_exceptions.specifics.InternalMappedException;

import java.util.List;
import java.util.stream.Stream;

public class ScopeBasedAutoauth {

    public static void handle(ExecutionContext executionContext, UseCase useCase){
        var useCaseMetadata = useCase.getUseCaseMetadata();
        if (Boolean.TRUE.equals(useCaseMetadata.isProtected()) && useCaseMetadata.getScopes().length > 0){
            var stepInsight = executionContext.addStepInsightsOf("ScopeBasedAutoauth");
            var notAllowed = !allows(getActorOutta(executionContext), useCaseMetadata.getScopes());
            if (notAllowed){
                var notAllowedException = new NotAllowedMappedException(useCase);
                stepInsight.complete(notAllowedException);
                throw notAllowedException;
            }
            else{
                stepInsight.complete();
            }
        }
    }

    private static Actor getActorOutta(ExecutionContext executionContext) {
        return executionContext.getActor()
            .orElseThrow(() -> new InternalMappedException(
                "No actor instance provided",
                "For executing protected use cases, you must provide an instance of Actor via the ExecutionContext object. Please fix it and try again."
            ));
    }

    private static boolean allows(Actor actor, String[] requiredScopes){
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
