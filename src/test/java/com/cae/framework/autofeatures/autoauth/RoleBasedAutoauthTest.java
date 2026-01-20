package com.cae.framework.autofeatures.autoauth;

import com.cae.context.ExecutionContext;
import com.cae.context.actors.Actor;
import com.cae.framework.autofeatures.autoauth.annotations.Edge;
import com.cae.framework.autofeatures.autoauth.annotations.ResourceIdentifier;
import com.cae.framework.autofeatures.autoauth.annotations.ResourceOwnerIdentifier;
import com.cae.framework.autofeatures.autolog.AutologProvider;
import com.cae.framework.autofeatures.autolog.Logger;
import com.cae.framework.use_cases.FunctionUseCase;
import com.cae.framework.use_cases.io.UseCaseInput;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.mapped_exceptions.specifics.NotAuthorizedMappedException;
import lombok.Getter;
import lombok.Setter;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.MockedAutofeaturesRunnerProvider;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@ExtendWith(MockitoExtension.class)
class RoleBasedAutoauthTest {

    @Mock
    Logger logger;
    @Mock
    RoleRetriever roleRetriever;
    @Mock
    Actor actor;
    @Mock
    ResourceOwnershipRetriever resourceOwnershipRetriever;

    String actorId = UUID.randomUUID().toString();
    ExecutionContext executionContext;
    RoleBasedProtectedUseCaseWithResourceOwnerId resourceOwnerIdBasedAutoauthUseCase;
    RoleBasedProtectedUseCaseWithResourceIdButNoOwnershipRetriever resourceIdBasedAutoauthUseCaseWithNoOwnershipRetriever;
    RoleBasedProtectedUseCaseWithResourceIdWithOwnershipRetriever resourceIdBasedAutoauthUseCaseWithOwnershipRetriever;

    @BeforeEach
    void setup(){
        AutologProvider.SINGLETON.setProvidedInstance(this.logger);
        MockedAutofeaturesRunnerProvider.run();
        this.executionContext = ExecutionContext.ofNew(this.actor);
        this.resourceOwnerIdBasedAutoauthUseCase = new RoleBasedProtectedUseCaseWithResourceOwnerId();
        this.resourceIdBasedAutoauthUseCaseWithNoOwnershipRetriever = new RoleBasedProtectedUseCaseWithResourceIdButNoOwnershipRetriever();
        this.resourceIdBasedAutoauthUseCaseWithOwnershipRetriever = new RoleBasedProtectedUseCaseWithResourceIdWithOwnershipRetriever(this.resourceOwnershipRetriever);
        RoleRetrieverRegistry.SINGLETON.setDefaultRoleRetriever(this.roleRetriever);

    }

    @Test
    @DisplayName("Should allow execution when a role retrieved for the actor allows the action with same ID as the use case")
    void shouldAllowExecutionWhenARoleRetrievedForTheActorAllowsTheActionWithSameIDAsTheUseCase(){
        Mockito.when(this.actor.getId()).thenReturn(this.actorId);
        var useCaseId = this.resourceOwnerIdBasedAutoauthUseCase.getUseCaseMetadata().getId();
        var allowingRole = ConcreteRole.builder()
                .roleIdentifier(UUID.randomUUID().toString())
                .ownerIdentifier("theOwner")
                .statements(List.of(ConcreteRoleStatement.builder().allows(true).actionIds(List.of(useCaseId)).build()))
                .build();
        List<RoleContract> rolesRetrieved = List.of(allowingRole);
        Mockito.when(this.roleRetriever.getRolesBy(this.actorId, this.executionContext)).thenReturn(rolesRetrieved);
        var theInput = new RoleBasedProtectedUseCaseWithResourceOwnerId.Input();
        Assertions.assertDoesNotThrow(() -> this.resourceOwnerIdBasedAutoauthUseCase.execute(theInput, this.executionContext));
    }

    @Test
    @DisplayName("Should throw NotAuthorizedException when the retrieved role has no allowing statements for the action of same ID as the use case")
    void shouldThrowNotAuthorizedExceptionWhenTheRetrievedRoleHasNoAllowingStatementsForTheActionOfSameIDAsTheUseCase(){
        Mockito.when(this.actor.getId()).thenReturn(this.actorId);
        var notAllowingRole = ConcreteRole.builder()
                .roleIdentifier(UUID.randomUUID().toString())
                .ownerIdentifier("theOwner")
                .statements(new ArrayList<>())
                .build();
        List<RoleContract> rolesRetrieved = List.of(notAllowingRole);
        Mockito.when(this.roleRetriever.getRolesBy(this.actorId, this.executionContext)).thenReturn(rolesRetrieved);
        var theInput = new RoleBasedProtectedUseCaseWithResourceOwnerId.Input();
        Assertions.assertThrows(NotAuthorizedMappedException.class, () -> this.resourceOwnerIdBasedAutoauthUseCase.execute(theInput, this.executionContext));
    }

    @Test
    @DisplayName("If no default role retriever is provided should throw InternalMappedException")
    void ifNoDefaultRoleRetrieverIsProvidedShouldThrowInternalMappedException(){
        var registry = RoleRetrieverRegistry.SINGLETON.setDefaultRoleRetriever(null);
        Assertions.assertEquals(RoleRetrieverRegistry.SINGLETON, registry);
        var theInput = new RoleBasedProtectedUseCaseWithResourceOwnerId.Input();
        Assertions.assertThrows(InternalMappedException.class, () -> this.resourceOwnerIdBasedAutoauthUseCase.execute(theInput, this.executionContext));
    }

    @Test
    @DisplayName("Should throw InternalMappedException when use case input is based on ResourceIdentifier and no ResourceOwnershipRetriever is provided")
    void shouldThrowInternalMappedExceptionWhenUseCaseInputIsBasedOnResourceIdentifierAndNoResourceOwnershipRetrieverIsProvided(){
        var theInput = new RoleBasedProtectedUseCaseWithResourceIdButNoOwnershipRetriever.Input();
        Assertions.assertThrows(InternalMappedException.class, () -> this.resourceIdBasedAutoauthUseCaseWithNoOwnershipRetriever.execute(theInput, this.executionContext));
    }

    @Test
    @DisplayName("When the use case works around resource IDs and the resource ownership retriever is provided and finds the owner of the resource should run without throwing")
    void whenUseCaseWorksAroundResourceIDsAndTheResourceOwnershipRetrieverIsProvidedAndFindsTheOwnerOfTheResourceShouldRunWithoutThrowing(){
        Mockito.when(this.actor.getId()).thenReturn(this.actorId);
        var theInput = new RoleBasedProtectedUseCaseWithResourceIdWithOwnershipRetriever.Input();
        var ownerId = "ownerId";
        Mockito.when(this.resourceOwnershipRetriever.findByResourceId(theInput.resourceId)).thenReturn(Optional.of(ownerId));
        var useCaseId = this.resourceIdBasedAutoauthUseCaseWithOwnershipRetriever.getUseCaseMetadata().getId();
        var allowingRole = ConcreteRole.builder()
                .roleIdentifier(UUID.randomUUID().toString())
                .ownerIdentifier(ownerId)
                .statements(List.of(ConcreteRoleStatement.builder().allows(true).actionIds(List.of(useCaseId)).build()))
                .build();
        List<RoleContract> rolesRetrieved = List.of(allowingRole);
        Mockito.when(this.roleRetriever.getRolesBy(this.actorId, this.executionContext)).thenReturn(rolesRetrieved);
        Assertions.assertDoesNotThrow(() -> this.resourceIdBasedAutoauthUseCaseWithOwnershipRetriever.execute(theInput, this.executionContext));
    }

    @Test
    @DisplayName("When actor has roles that allow the action to be executed but the ownership is from another entity should throw NotAuthorizedMappedException")
    void whenActorHasRolesThatAllowTheActionToBeExecutedButTheOwnershipIsFromAnotherEntityShouldThrowNotAuthorizedMappedException(){
        Mockito.when(this.actor.getId()).thenReturn(this.actorId);
        var theInput = new RoleBasedProtectedUseCaseWithResourceIdWithOwnershipRetriever.Input();
        var resourceOwnerId = "resourceOwnerId";
        var roleOwnerId = "roleOwnerId";
        Mockito.when(this.resourceOwnershipRetriever.findByResourceId(theInput.resourceId)).thenReturn(Optional.of(resourceOwnerId));
        var useCaseId = this.resourceIdBasedAutoauthUseCaseWithOwnershipRetriever.getUseCaseMetadata().getId();
        var allowingRole = ConcreteRole.builder()
                .roleIdentifier(UUID.randomUUID().toString())
                .ownerIdentifier(roleOwnerId)
                .statements(List.of(ConcreteRoleStatement.builder().allows(true).actionIds(List.of(useCaseId)).build()))
                .build();
        List<RoleContract> rolesRetrieved = List.of(allowingRole);
        Mockito.when(this.roleRetriever.getRolesBy(this.actorId, this.executionContext)).thenReturn(rolesRetrieved);
        Assertions.assertThrows(
                NotAuthorizedMappedException.class,
                () -> this.resourceIdBasedAutoauthUseCaseWithOwnershipRetriever.execute(theInput, this.executionContext)
        );
    }

    @Test
    @DisplayName("Should throw InternalMappedException when no Actor instance is provided in the execution context")
    void shouldThrowInternalMappedExceptionWhenNoActorInstanceIsProvidedInTheExecutionContext(){
        var wrongExecContext = ExecutionContext.ofNew();
        var theInput = new RoleBasedProtectedUseCaseWithResourceOwnerId.Input();
        Assertions.assertThrows(InternalMappedException.class, () -> this.resourceOwnerIdBasedAutoauthUseCase.execute(theInput, wrongExecContext));
    }

    @Test
    @DisplayName("When the use case hasn't declared a ResourceIdentifier nor a ResourceOwnerIdentifier should throw when executing")
    void whenTheUseCaseHasNotDeclaredResourceIdentifierNorResourceOwnerIdentifierShouldThrowWhenExecuting(){
        var wrongUseCase = new SomeWrongRoleProtectedUseCase();
        var input = new SomeWrongRoleProtectedUseCase.Input();
        Assertions.assertThrows(InternalMappedException.class, () -> wrongUseCase.execute(input, this.executionContext));
    }

    @Edge(actionId = "some_action_id")
    public static class RoleBasedProtectedUseCaseWithResourceOwnerId
            extends FunctionUseCase<RoleBasedProtectedUseCaseWithResourceOwnerId.Input, RoleBasedProtectedUseCaseWithResourceOwnerId.Output>{

        @Getter
        @Setter
        public static class Input extends UseCaseInput {
            @ResourceOwnerIdentifier
            private String ownerId = "theOwner";
        }
        public static class Output{}

        @Override
        protected Output applyInternalLogic(Input input, ExecutionContext context) {
            return new Output();
        }

    }

    @Edge(actionId = "other_action_id")
    public static class RoleBasedProtectedUseCaseWithResourceIdButNoOwnershipRetriever
            extends FunctionUseCase<RoleBasedProtectedUseCaseWithResourceIdButNoOwnershipRetriever.Input, RoleBasedProtectedUseCaseWithResourceIdButNoOwnershipRetriever.Output>{

        @Getter
        @Setter
        public static class Input extends UseCaseInput {
            @ResourceIdentifier
            private String resourceId = "resourceId";
        }
        public static class Output{}

        @Override
        protected Output applyInternalLogic(Input input, ExecutionContext context) {
            return new Output();
        }

    }

    @Edge(actionId = "other_action_id")
    public static class RoleBasedProtectedUseCaseWithResourceIdWithOwnershipRetriever
            extends FunctionUseCase<RoleBasedProtectedUseCaseWithResourceIdWithOwnershipRetriever.Input, RoleBasedProtectedUseCaseWithResourceIdWithOwnershipRetriever.Output>{

        public RoleBasedProtectedUseCaseWithResourceIdWithOwnershipRetriever(ResourceOwnershipRetriever resourceOwnershipRetriever){
            super(resourceOwnershipRetriever);
        }

        @Getter
        @Setter
        public static class Input extends UseCaseInput {
            @ResourceIdentifier
            private String resourceId = "resourceId";
        }
        public static class Output{}

        @Override
        protected Output applyInternalLogic(Input input, ExecutionContext context) {
            return new Output();
        }

    }

    @Edge(actionId = "other_action_id")
    public static class SomeWrongRoleProtectedUseCase
            extends FunctionUseCase<SomeWrongRoleProtectedUseCase.Input, SomeWrongRoleProtectedUseCase.Output>{

        @Getter
        @Setter
        public static class Input extends UseCaseInput {}
        public static class Output{}

        @Override
        protected Output applyInternalLogic(Input input, ExecutionContext context) {
            return new Output();
        }

    }


}
