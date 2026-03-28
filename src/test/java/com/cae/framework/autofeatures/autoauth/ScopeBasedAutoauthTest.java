package com.cae.framework.autofeatures.autoauth;

import com.cae.context.ExecutionContext;
import com.cae.context.actors.Actor;
import com.cae.framework.use_cases.boundaries.Internal;
import com.cae.framework.autofeatures.autoauth.exceptions.NotAllowedMappedException;
import com.cae.framework.use_cases.RunnableUseCase;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

@ExtendWith(MockitoExtension.class)
class ScopeBasedAutoauthTest {

    @Mock
    Actor actor;

    ExecutionContext executionContext;

    @BeforeEach
    void setup(){
        this.executionContext = ExecutionContext.ofNew(this.actor);
        this.executionContext.setSubjectAndStartTracking("ScopeBasedAutoauthTesting", true);
    }

    @Test
    @DisplayName("Should allow execution for SimpleScopeBasedProtectedUseCase when Actor has the scope")
    void shouldAllowExecutionForSimpleScopeBasedProtectedUseCaseWhenActorHasTheScope(){
        var useCase = new SimpleScopeBasedProtectedUseCase();
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("SINGLE-SCOPE"));
        Assertions.assertDoesNotThrow(() -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
    }

    @Test
    @DisplayName("Should refuse execution for SimpleScopeBasedProtectedUseCase when Actor doesn't have the scope")
    void shouldRefuseExecutionForSimpleScopeBasedProtectedUseCaseWhenActorDoesNotHaveTheScope(){
        var useCase = new SimpleScopeBasedProtectedUseCase();
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("SOME-OTHER-SCOPE"));
        Assertions.assertThrows(NotAllowedMappedException.class, () -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
    }

    @Internal(scopes = "SINGLE-SCOPE")
    public static class SimpleScopeBasedProtectedUseCase extends RunnableUseCase{
        @Override
        protected void applyInternalLogic(ExecutionContext context) {}
    }

    @Test
    @DisplayName("Should allow execution for SimpleLogicalAndScopeBasedProtectedUseCase when Actor has the scope")
    void shouldAllowExecutionForSimpleLogicalAndScopeBasedProtectedUseCaseWhenActorHasTheScope(){
        var useCase = new SimpleLogicalAndScopeBasedProtectedUseCase();
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("A-SCOPE", "ANOTHER-SCOPE"));
        Assertions.assertDoesNotThrow(() -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
    }

    @Test
    @DisplayName("Should refuse execution for SimpleLogicalAndScopeBasedProtectedUseCase when Actor doesn't have one of the scopes")
    void shouldRefuseExecutionForSimpleLogicalAndScopeBasedProtectedUseCaseWhenActorDoesNotHaveTheScopes(){
        var useCase = new SimpleLogicalAndScopeBasedProtectedUseCase();
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("ANOTHER-SCOPE"));
        Assertions.assertThrows(NotAllowedMappedException.class, () -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("A-SCOPE"));
        Assertions.assertThrows(NotAllowedMappedException.class, () -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
    }

    @Internal(scopes = "A-SCOPE && ANOTHER-SCOPE")
    public static class SimpleLogicalAndScopeBasedProtectedUseCase extends RunnableUseCase{
        @Override
        protected void applyInternalLogic(ExecutionContext context) {}
    }

    @Test
    @DisplayName("Should allow execution for SimpleLogicalOrScopeBasedProtectedUseCase when Actor has one of the scopes")
    void shouldAllowExecutionForSimpleLogicalOrScopeBasedProtectedUseCaseWhenActorHasOneOfTheScopes(){
        var useCase = new SimpleLogicalOrScopeBasedProtectedUseCase();
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("ONE"));
        Assertions.assertDoesNotThrow(() -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("OR-ANOTHER"));
        Assertions.assertDoesNotThrow(() -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
    }

    @Internal(scopes = "ONE || OR-ANOTHER")
    public static class SimpleLogicalOrScopeBasedProtectedUseCase extends RunnableUseCase{
        @Override
        protected void applyInternalLogic(ExecutionContext context) {}
    }

    @Test
    @DisplayName("Should allow execution for SimpleLogicalNotScopeBasedProtectedUseCase when Actor does NOT have the blocked scope")
    void shouldAllowExecutionForSimpleLogicalNotScopeBasedProtectedUseCaseWhenActorDoesNotHaveBlockedScope(){
        var useCase = new SimpleLogicalNotScopeBasedProtectedUseCase();
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("USER"));
        Assertions.assertDoesNotThrow(() -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("ADMIN"));
        Assertions.assertDoesNotThrow(() -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
    }

    @Test
    @DisplayName("Should refuse execution for SimpleLogicalNotScopeBasedProtectedUseCase when Actor has the blocked scope")
    void shouldRefuseExecutionForSimpleLogicalNotScopeBasedProtectedUseCaseWhenActorHasBlockedScope(){
        var useCase = new SimpleLogicalNotScopeBasedProtectedUseCase();
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("BLOCKED"));
        Assertions.assertThrows(NotAllowedMappedException.class, () -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("ADMIN", "BLOCKED"));
        Assertions.assertThrows(NotAllowedMappedException.class, () -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
    }

    @Internal(scopes = "!BLOCKED")
    public static class SimpleLogicalNotScopeBasedProtectedUseCase extends RunnableUseCase{
        @Override
        protected void applyInternalLogic(ExecutionContext context) {}
    }

    @Test
    @DisplayName("Should allow execution for LogicalAndNotScopeBasedProtectedUseCase when Actor has required scope and not the blocked one")
    void shouldAllowExecutionForLogicalAndNotScopeBasedProtectedUseCaseWhenActorHasRequiredAndNotBlocked(){
        var useCase = new LogicalAndNotScopeBasedProtectedUseCase();
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("ADMIN"));
        Assertions.assertDoesNotThrow(() -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
    }

    @Test
    @DisplayName("Should refuse execution for LogicalAndNotScopeBasedProtectedUseCase when Actor has blocked scope or lacks required scope")
    void shouldRefuseExecutionForLogicalAndNotScopeBasedProtectedUseCaseWhenActorHasBlockedOrLacksRequired(){
        var useCase = new LogicalAndNotScopeBasedProtectedUseCase();
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("ADMIN", "BLOCKED"));
        Assertions.assertThrows(NotAllowedMappedException.class, () -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("BLOCKED"));
        Assertions.assertThrows(NotAllowedMappedException.class, () -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
    }

    @Internal(scopes = "ADMIN && !BLOCKED")
    public static class LogicalAndNotScopeBasedProtectedUseCase extends RunnableUseCase{
        @Override
        protected void applyInternalLogic(ExecutionContext context) {}
    }

    @Test
    @DisplayName("Should allow execution for LogicalOrAndNotScopeBasedProtectedUseCase when Actor has one role and not blocked")
    void shouldAllowExecutionForLogicalOrAndNotScopeBasedProtectedUseCaseWhenActorHasRoleAndNotBlocked(){
        var useCase = new LogicalOrAndNotScopeBasedProtectedUseCase();
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("ADMIN"));
        Assertions.assertDoesNotThrow(() -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("MANAGER"));
        Assertions.assertDoesNotThrow(() -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
    }

    @Test
    @DisplayName("Should refuse execution for LogicalOrAndNotScopeBasedProtectedUseCase when Actor has blocked scope")
    void shouldRefuseExecutionForLogicalOrAndNotScopeBasedProtectedUseCaseWhenActorHasBlockedScope(){
        var useCase = new LogicalOrAndNotScopeBasedProtectedUseCase();
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("ADMIN", "BLOCKED"));
        Assertions.assertThrows(NotAllowedMappedException.class, () -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
    }

    @Internal(scopes = "(ADMIN || MANAGER) && !BLOCKED")
    public static class LogicalOrAndNotScopeBasedProtectedUseCase extends RunnableUseCase{
        @Override
        protected void applyInternalLogic(ExecutionContext context) {}
    }

    @Test
    @DisplayName("Should allow execution for ComplexLogicalAndScopeBasedProtectedUseCase when Actor has all of the required scopes")
    void shouldAllowExecutionForComplexLogicalAndScopeBasedProtectedUseCaseWhenActorHasAllOfTheRequiredScopes(){
        var useCase = new ComplexLogicalAndScopeBasedProtectedUseCase();
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("A-SCOPE", "ANOTHER-SCOPE", "AND-AGAIN", "ONE-MORE"));
        Assertions.assertDoesNotThrow(() -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("A-SCOPE", "ANOTHER-SCOPE", "AND-AGAIN", "OR-THIS"));
        Assertions.assertDoesNotThrow(() -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
    }

    @Internal(scopes = "A-SCOPE && ANOTHER-SCOPE && AND-AGAIN && (ONE-MORE || OR-THIS)")
    public static class ComplexLogicalAndScopeBasedProtectedUseCase extends RunnableUseCase{
        @Override
        protected void applyInternalLogic(ExecutionContext context) {}
    }

    @Test
    @DisplayName("Should allow execution for ComplexLogicalOrScopeBasedProtectedUseCase when Actor has all of the required scopes")
    void shouldAllowExecutionForComplexLogicalOrScopeBasedProtectedUseCaseWhenActorHasAllOfTheRequiredScopes(){
        var useCase = new ComplexLogicalOrScopeBasedProtectedUseCase();
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("ONE", "AND-THIS"));
        Assertions.assertDoesNotThrow(() -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("OR-ANOTHER", "AND-THIS"));
        Assertions.assertDoesNotThrow(() -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("OR-THIS", "AND-THIS"));
        Assertions.assertDoesNotThrow(() -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("OR-THAT", "AND-THIS"));
        Assertions.assertDoesNotThrow(() -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
    }

    @Internal(scopes = "(ONE || OR-ANOTHER || OR-THIS || OR-THAT) && AND-THIS")
    public static class ComplexLogicalOrScopeBasedProtectedUseCase extends RunnableUseCase{
        @Override
        protected void applyInternalLogic(ExecutionContext context) {}
    }

    @Test
    @DisplayName("Should evaluate nested logical expressions correctly")
    void shouldEvaluateNestedLogicalExpressionsCorrectly(){
        var useCase = new NestedScopeBasedProtectedUseCase();
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("ADMIN", "VISIT"));
        Assertions.assertDoesNotThrow(() -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("MANAGER", "USER", "VISIT"));
        Assertions.assertDoesNotThrow(() -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
        Mockito.when(this.actor.getScopes()).thenReturn(List.of("ADMIN"));
        Assertions.assertThrows(NotAllowedMappedException.class, () -> ScopeBasedAutoauth.handle(this.executionContext, useCase));
    }

    @Internal(scopes = "(ADMIN || MANAGER) && (USER || VISIT)")
    public static class NestedScopeBasedProtectedUseCase extends RunnableUseCase{
        @Override
        protected void applyInternalLogic(ExecutionContext context) {}
    }

}
