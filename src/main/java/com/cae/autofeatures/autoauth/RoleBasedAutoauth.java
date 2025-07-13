package com.cae.autofeatures.autoauth;

import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.mapped_exceptions.specifics.NotAuthorizedMappedException;
import com.cae.mapped_exceptions.specifics.NotFoundMappedException;
import com.cae.use_cases.UseCase;
import com.cae.use_cases.UseCaseWithInput;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.contexts.actors.Actor;
import com.cae.use_cases.io.UseCaseInput;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class RoleBasedAutoauth {

    public static void handle(
            UseCaseInput input,
            ExecutionContext context,
            UseCaseWithInput useCase){
        if (Boolean.TRUE.equals(((UseCase) useCase).getUseCaseMetadata().getRoleProtectionEnabled())){
            var stepInsight = context.addStepInsightsOf("RoleBasedAutoauth");
            try{
                var resourceOwnerId = input.getResourceOwnerIdentifier();
                if (resourceOwnerId.isPresent()) handleByResourceOwnerId(resourceOwnerId.get(), context, (UseCase) useCase);
                else handleByResourceId(input, context, useCase);
                stepInsight.complete();
            } catch (Exception exception){
                stepInsight.complete(exception);
                throw exception;
            }
        }
    }

    private static void handleByResourceId(
            UseCaseInput input,
            ExecutionContext context,
            UseCaseWithInput useCase) {
        var resourceId = getResourceIdOutta(input, (UseCase) useCase);
        var resourceOwnershipRetriever = getResourceOwnershipRetrieverOutta(useCase);
        var resourceOwnerId = getOwnerUsing(resourceId, resourceOwnershipRetriever);
        handleByResourceOwnerId(resourceOwnerId, context, (UseCase) useCase);
    }

    private static String getResourceIdOutta(UseCaseInput input, UseCase useCase) {
        return input.getResourceIdentifier().orElseThrow(() -> new InternalMappedException(
                "Problem trying to get the resource identifier from input '" + input.getClass().getSimpleName() + "'",
                "The use case '" + useCase.getUseCaseMetadata().getName() + "' was annotated with RoleBasedProtection, so it is mandatory that " +
                        "either a resource identifier or a resource owner identifier is provided within the use case input instance. " +
                        "This is done by annotating one of the input fields with @ResourceIdentifier (for indirect ownership validation) or @ResourceOwnerIdentifier (for direct ownership validation)"
        ));
    }

    private static ResourceOwnershipRetriever getResourceOwnershipRetrieverOutta(UseCaseWithInput useCase) {
        return useCase.getResourceOwnershipRetriever().orElseThrow(() -> new InternalMappedException(
                "Problem trying to get the resource ownership retriever for the use case '" +  ((UseCase) useCase).getUseCaseMetadata().getName() + "'",
                "This use case instance didn't have in its input object a field annotated with @ResourceOwnerIdentifier, so the retrieval of the resource owner must be done " +
                        "by the @ResourceIdentifier field. This is done by calling the ResourceOwnershipRetriever API. An instance of it must be provided at the use case constructor level."
        ));
    }

    private static String getOwnerUsing(String resourceId, ResourceOwnershipRetriever resourceOwnershipRetriever) {
        return resourceOwnershipRetriever.findByResourceId(resourceId).orElseThrow(() -> new NotFoundMappedException(
                "The resource owner ID was not found for the resource ID of '" + resourceId + "'.",
                "It is not possible to proceed with the role-based authorization since the ownership can't be validated."
        ));
    }

    private static void handleByResourceOwnerId(
            String resourceOwnerId,
            ExecutionContext context,
            UseCase useCase) {
        var useCaseId = useCase.getUseCaseMetadata().getId();
        var actor = getActorOutta(context);
        var actorRoles = getRolesFor(actor, useCaseId, context);
        var rolesThatMatchAndAllowTheUseCase = findOutWhichRolesMatch(actorRoles, useCaseId);
        if (rolesThatMatchAndAllowTheUseCase.isEmpty())
            throw new NotAuthorizedMappedException(
                    "The Actor of ID " + actor.getId() + " had no roles allowing the execution of the use case " + useCase.getUseCaseMetadata().getName() + "."
            );
        var noMatchingRoleSharesSameOwnership = rolesThatMatchAndAllowTheUseCase.stream().noneMatch(role -> role.getOwnerIdentifier().equals(resourceOwnerId));
        if (noMatchingRoleSharesSameOwnership)
            throw new NotAuthorizedMappedException(
                    "The Actor of ID " + actor.getId() + " has allowing roles, but none matched the ownership with the resource"
            );
    }

    private static Actor getActorOutta(ExecutionContext context){
        return context.getActor().orElseThrow(() ->
                new InternalMappedException(
                        "No Actor instance provided.",
                        "An Actor instance must be provided within the ExecutionContext object when the use case is protected."
                )
        );
    }

    private static List<RoleContract> getRolesFor(Actor actor, String useCaseId, ExecutionContext context) {
        return RoleRetrieverRegistry.SINGLETON.getRoleRetrieverByUseCaseId(useCaseId)
                .orElseGet(() -> RoleRetrieverRegistry.SINGLETON.getDefaultRetriever().orElseThrow(() ->
                        new InternalMappedException(
                                "Problem trying to retrieve actor's roles: no instance of RoleRetriever provided.",
                                "To fix it, either provide a default instance via the RoleRetrieverRegistry or a specific one for this use case, also via the same API. " +
                                        "The default instance is for the scenario which doesn't make sense to at the level of each Use Case specify the same role retriever. However, " +
                                        "if a Use Case specifically needs a different role retriever because its actor is special in relationship to other Use Cases, then it makes sense to provide a " +
                                        "specific instance at the Use Case's constructor level."
                        )
                ))
                .getRolesBy(actor.getId(), context);
    }

    private static List<RoleContract> findOutWhichRolesMatch(List<RoleContract> actorRoles, String useCaseId) {
        var finalResult = new ArrayList<RoleContract>();
        for (var role : actorRoles){
            var roleHasMatchingProperties = role.getStatements()
                    .stream()
                    .anyMatch(
                            statement -> statement.getActionIds().stream().anyMatch(statementUseCaseId -> statementUseCaseId.equals(useCaseId))
                            && statement.allows()
                    );
            if (roleHasMatchingProperties) finalResult.add(role);
        }
        return finalResult;
    }

}
