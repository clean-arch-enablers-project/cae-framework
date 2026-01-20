package com.cae.framework.use_cases.metadata;

import com.cae.context.ExecutionContext;
import com.cae.framework.autofeatures.autoauth.AutoauthModes;
import com.cae.framework.use_cases.boundaries.Edge;
import com.cae.framework.use_cases.ConsumerUseCase;
import com.cae.framework.use_cases.RunnableUseCase;
import com.cae.framework.use_cases.io.UseCaseInput;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import lombok.Getter;
import lombok.Setter;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.normal_use_cases.SomeNormalFunctionUseCase;

import java.util.stream.Stream;

@ExtendWith(MockitoExtension.class)
class UseCaseMetadataTest {

    public static final String SCOPE_ONE = "teste";
    public static final String SCOPE_TWO = "another test";
    public static final String ACTION_ID = "334455";

    @Test
    @DisplayName("Should be able to correctly extract the scopes set to the use case")
    void shouldBeAbleToCorrectlyExtractTheScopesSetToTheUseCase(){
        var metadata = UseCaseMetadata.of(new SomeScopeBasedProtectedUseCase());
        var extractedScopes = metadata.getScopes();
        Assertions.assertEquals(2, extractedScopes.length);
        var containsScopeOne = Stream.of(extractedScopes)
                        .anyMatch(scope -> scope.equalsIgnoreCase(SCOPE_ONE));
        var containsScopeTwo = Stream.of(extractedScopes)
                .anyMatch(scope -> scope.equalsIgnoreCase(SCOPE_TWO));
        Assertions.assertTrue(containsScopeOne);
        Assertions.assertTrue(containsScopeTwo);
    }

    @Test
    @DisplayName("Should return false when the use case class is not annotated with the RoleBasedProtection")
    void shouldReturnFalseWhenTheUseCaseClassIsNotAnnotatedWithTheRoleBasedProtection(){
        var metadata = UseCaseMetadata.of(new SomeNormalFunctionUseCase());
        Assertions.assertFalse(metadata.isRoleProtectionEnabled());
    }

    @Test
    @DisplayName("Should return true when the use case class is annotated with the RoleBasedProtection")
    void shouldReturnTrueWhenTheUseCaseClassIsAnnotatedWithTheRoleBasedProtection(){
        var metadata = UseCaseMetadata.of(new SomeRoleBasedProtectedUseCase());
        Assertions.assertTrue(metadata.isRoleProtectionEnabled());
    }

    @Test
    @DisplayName("Should be able to extract the action ID of a use case from UseCaseAsAction")
    void shouldBeAbleToExtractTheActionIdOfUseCaseFromUseCaseAsAction(){
        var metadata = UseCaseMetadata.of(new SomeUseCaseAsAction());
        var extractedActionId = metadata.getId();
        Assertions.assertEquals(ACTION_ID, extractedActionId);
    }

    @Test
    @DisplayName("When use case has no ID set via UseCaseAsAction should return empty string")
    void whenUseCaseHasNoIdSetViaUseCaseAsActionShouldReturnEmptyString(){
        var metadata = UseCaseMetadata.of(new SomeNormalFunctionUseCase());
        Assertions.assertTrue(metadata.getId().isEmpty());
    }

    @Test
    @DisplayName("Should be able to return 334455 as action ID")
    void shouldBeAbleToReturn3333AsActionIdForRoleBasedProtection(){
        var expected = "334455";
        var metadata = UseCaseMetadata.of(new SomeRoleBasedProtectedUseCase());
        Assertions.assertEquals(expected, metadata.getId());
    }

    @Test
    @DisplayName("Should throw InternalMappedException when RoleBasedProtection is used but no action ID is provided at all")
    void shouldThrowInternalMappedExceptionWhenRoleBasedProtectionIsUsedButNoActionIDIsProvidedAtAll(){
        Assertions.assertThrows(InternalMappedException.class, SomeProblematicRoleBasedProtectedUseCaseWithoutActionId::new);
    }

    @Test
    @DisplayName("Should instantiate UseCaseMetadata object as expected")
    void shouldInstantiateUseCaseMetadataObjectAsExpected(){
        var someFunctionUseCase = new SomeNormalFunctionUseCase();
        var metadata = UseCaseMetadata.of(someFunctionUseCase);
        Assertions.assertEquals("SomeNormalFunctionUseCase", metadata.getName());
        Assertions.assertFalse(metadata.isProtected());
        Assertions.assertEquals(0, metadata.getScopes().length);
        Assertions.assertFalse(metadata.isRoleProtectionEnabled());
        Assertions.assertTrue(metadata.getId().isEmpty());
    }

    @Edge(scopes = {SCOPE_ONE, SCOPE_TWO})
    public static class SomeScopeBasedProtectedUseCase extends RunnableUseCase{
        @Override
        protected void applyInternalLogic(ExecutionContext context) {
            //does nothing
        }
    }

    @Edge(actionId = "334455", autoauth = AutoauthModes.RBAC)
    public static class SomeRoleBasedProtectedUseCase extends ConsumerUseCase<SomeRoleBasedProtectedUseCase.Input>{

        @Getter
        @Setter
        public static class Input extends UseCaseInput {}

        @Override
        protected void applyInternalLogic(Input input, ExecutionContext context) {
            //does nothing
        }

    }

    @Edge(actionId = ACTION_ID)
    public static class SomeUseCaseAsAction extends ConsumerUseCase<SomeUseCaseAsAction.Input>{

        @Getter
        @Setter
        public static class Input extends UseCaseInput {}

        @Override
        protected void applyInternalLogic(Input input, ExecutionContext context) {
            //does nothing
        }

    }

    @Edge(autoauth = AutoauthModes.RBAC)
    public static class SomeProblematicRoleBasedProtectedUseCaseWithoutActionId extends ConsumerUseCase<SomeProblematicRoleBasedProtectedUseCaseWithoutActionId.Input>{

        public static class Input extends UseCaseInput{ }

        @Override
        protected void applyInternalLogic(Input input, ExecutionContext context) {
            //does nothing
        }
    }

}
