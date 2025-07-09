package com.cae.use_cases.metadata;

import com.cae.autofeatures.autoauth.annotations.RoleBasedProtection;
import com.cae.autofeatures.autoauth.annotations.ScopeBasedProtection;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.ConsumerUseCase;
import com.cae.use_cases.RunnableUseCase;
import com.cae.use_cases.UseCaseAsAction;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.io.UseCaseInput;
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
        var extractedScopes = UseCaseMetadata.getRequiredScopesOutta(SomeScopeBasedProtectedUseCase.class);
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
        Assertions.assertFalse(UseCaseMetadata.findOutWhetherOrNotRoleProtected(SomeNormalFunctionUseCase.class));
    }

    @Test
    @DisplayName("Should return true when the use case class is annotated with the RoleBasedProtection")
    void shouldReturnTrueWhenTheUseCaseClassIsAnnotatedWithTheRoleBasedProtection(){
        Assertions.assertTrue(UseCaseMetadata.findOutWhetherOrNotRoleProtected(SomeRoleBasedProtectedUseCase.class));
    }

    @Test
    @DisplayName("Should be able to extract the action ID of a use case from UseCaseAsAction")
    void shouldBeAbleToExtractTheActionIdOfUseCaseFromUseCaseAsAction(){
        var extractedActionId = UseCaseMetadata.getIdByUseCaseIdAnnotation(SomeUseCaseAsAction.class);
        Assertions.assertEquals(ACTION_ID, extractedActionId);
    }

    @Test
    @DisplayName("When use case has no ID set via UseCaseAsAction should return empty string")
    void whenUseCaseHasNoIdSetViaUseCaseAsActionShouldReturnEmptyString(){
        var result = UseCaseMetadata.getIdByUseCaseIdAnnotation(SomeNormalFunctionUseCase.class);
        Assertions.assertTrue(result.isEmpty());
    }

    @Test
    @DisplayName("Should be able to extract the action ID of a use case from RoleBasedProtection")
    void shouldBeAbleToExtractTheActionIdOfUseCaseFromRoleBasedProtection(){
        var extractedActionId = UseCaseMetadata.getIdByRoleBasedProtectedUseCaseAnnotation(SomeRoleBasedProtectedUseCase.class);
        Assertions.assertEquals(ACTION_ID, extractedActionId);
    }

    @Test
    @DisplayName("When use case has no ID set via RoleBasedProtection should return empty string")
    void whenUseCaseHasNoIdSetViaRoleBasedProtectionShouldReturnEmptyString(){
        var result = UseCaseMetadata.getIdByRoleBasedProtectedUseCaseAnnotation(SomeNormalFunctionUseCase.class);
        Assertions.assertTrue(result.isEmpty());
    }

    @Test
    @DisplayName("Should be able to return 334455 as action ID")
    void shouldBeAbleToReturn3333AsActionIdForRoleBasedProtection(){
        var expected = "334455";
        var result = UseCaseMetadata.getIdOutta(SomeRoleBasedProtectedUseCase.class, true);
        Assertions.assertEquals(expected, result);
    }

    @Test
    @DisplayName("Should be able to return 334455 as action ID")
    void shouldBeAbleToReturn3333AsActionIdForUseCaseAsAction(){
        var expected = "334455";
        var result = UseCaseMetadata.getIdOutta(SomeUseCaseAsAction.class, false);
        Assertions.assertEquals(expected, result);
    }

    @Test
    @DisplayName("Should throw InternalMappedException when RoleBasedProtection is used but no action ID is provided at all")
    void shouldThrowInternalMappedExceptionWhenRoleBasedProtectionIsUsedButNoActionIDIsProvidedAtAll(){
        Assertions.assertThrows(InternalMappedException.class, () -> UseCaseMetadata.getIdOutta(SomeProblematicRoleBasedProtectedUseCaseWithoutActionId.class, true));
    }

    @Test
    @DisplayName("Should instantiate UseCaseMetadata object as expected")
    void shouldInstantiateUseCaseMetadataObjectAsExpected(){
        var someFunctionUseCase = new SomeNormalFunctionUseCase();
        var metadata = UseCaseMetadata.of(someFunctionUseCase);
        Assertions.assertEquals("some_normal_function", metadata.getName());
        Assertions.assertFalse(metadata.getIsProtected());
        Assertions.assertEquals(0, metadata.getScope().length);
        Assertions.assertFalse(metadata.getRoleProtectionEnabled());
        Assertions.assertTrue(metadata.getId().isEmpty());
    }

    @ScopeBasedProtection(scope = {SCOPE_ONE, SCOPE_TWO})
    public static class SomeScopeBasedProtectedUseCase extends RunnableUseCase{
        @Override
        protected void applyInternalLogic(ExecutionContext context) {
            //does nothing
        }
    }

    @RoleBasedProtection(actionId = "334455")
    public static class SomeRoleBasedProtectedUseCase extends ConsumerUseCase<SomeRoleBasedProtectedUseCase.Input>{

        @Getter
        @Setter
        public static class Input extends UseCaseInput {}

        @Override
        protected void applyInternalLogic(Input input, ExecutionContext context) {
            //does nothing
        }

    }

    @UseCaseAsAction(actionId = "334455")
    public static class SomeUseCaseAsAction extends ConsumerUseCase<SomeUseCaseAsAction.Input>{

        @Getter
        @Setter
        public static class Input extends UseCaseInput {}

        @Override
        protected void applyInternalLogic(Input input, ExecutionContext context) {
            //does nothing
        }

    }

    @RoleBasedProtection
    public static class SomeProblematicRoleBasedProtectedUseCaseWithoutActionId extends ConsumerUseCase<SomeProblematicRoleBasedProtectedUseCaseWithoutActionId.Input>{

        public static class Input extends UseCaseInput{ }

        @Override
        protected void applyInternalLogic(Input input, ExecutionContext context) {
            //does nothing
        }
    }

}
