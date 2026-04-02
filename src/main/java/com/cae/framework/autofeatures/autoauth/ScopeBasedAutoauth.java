package com.cae.framework.autofeatures.autoauth;

import com.cae.context.ExecutionContext;
import com.cae.context.actors.Actor;
import com.cae.framework.autofeatures.autoauth.exceptions.NotAllowedMappedException;
import com.cae.framework.autofeatures.autoauth.scope_expression.ScopeExpression;
import com.cae.framework.use_cases.UseCase;
import com.cae.mapped_exceptions.specifics.InternalMappedException;

import java.util.List;

public class ScopeBasedAutoauth {

    public static void handle(ExecutionContext executionContext, UseCase useCase){
        var useCaseMetadata = useCase.getUseCaseMetadata();
        if (Boolean.TRUE.equals(useCaseMetadata.isProtected()) && useCaseMetadata.getScopeExpression() != null){
            var stepInsight = executionContext.addStepInsightsOf("ScopeBasedAutoauth");
            var notAllowed = !allows(getActorOutta(executionContext), useCaseMetadata.getScopeExpression());
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

    private static boolean allows(Actor actor, ScopeExpression requiredScopesExpression){
        List<String> providedScopes = actor.getScopes();
        return requiredScopesExpression.evaluate(providedScopes);
    }

}
