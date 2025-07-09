package com.cae.use_cases.contexts;

import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.contexts.actors.Actor;
import com.cae.use_cases.contexts.exceptions.CorrelationIdValueFormatException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.UUID;

@ExtendWith(MockitoExtension.class)
class ExecutionContextTest {

    @Test
    @DisplayName("Should instantiate with a UUID correlation ID set as expected")
    void shouldInstantiateWithAllCorrelationIDSetAsExpected(){
        var correlationId = UUID.randomUUID();
        var newExec = new ExecutionContext(correlationId);
        Assertions.assertEquals(correlationId, newExec.getCorrelationId());
    }

    @Test
    @DisplayName("Should instantiate with a string correlation ID set as expected")
    void shouldInstantiateWithAStringCorrelationIDSetAsExpected(){
        var correlationIdInStringFormat = UUID.randomUUID().toString();
        var newExec = ExecutionContext.of(correlationIdInStringFormat);
        Assertions.assertEquals(correlationIdInStringFormat, newExec.getCorrelationId().toString());
    }

    @Test
    @DisplayName("Should instantiate with a UUID and an Actor set as expected")
    void shouldInstantiateWithAUUIDAndAnActorSetAsExpected(){
        var correlationId = UUID.randomUUID();
        var actor = Mockito.mock(Actor.class);
        var execContext = new ExecutionContext(correlationId, actor);
        Assertions.assertEquals(correlationId, execContext.getCorrelationId());
        Assertions.assertTrue(execContext.getActor().isPresent());
        Assertions.assertEquals(actor, execContext.getActor().get());
    }

    @Test
    @DisplayName("Should instantiate with a string UUID and an Actor set as expected")
    void shouldInstantiateWithAStringUUIDAndAnActorSetAsExpected(){
        var correlationId = UUID.randomUUID().toString();
        var actor = Mockito.mock(Actor.class);
        var execContext = ExecutionContext.of(correlationId, actor);
        Assertions.assertEquals(correlationId, execContext.getCorrelationId().toString());
        Assertions.assertTrue(execContext.getActor().isPresent());
        Assertions.assertEquals(actor, execContext.getActor().get());
    }

    @Test
    @DisplayName("Should throw CorrelationIdValueFormatException if non-UUID value is provided")
    void shouldThrowCorrelationIdValueFormatExceptionIfNonUUIDValueIsProvided(){
        var nonUUIDStringValue = "some random string here";
        Assertions.assertThrows(CorrelationIdValueFormatException.class, () -> ExecutionContext.of(nonUUIDStringValue));
        var actor = Mockito.mock(Actor.class);
        Assertions.assertThrows(CorrelationIdValueFormatException.class, () -> ExecutionContext.of(nonUUIDStringValue, actor));
    }

    @Test
    @DisplayName("Should be able to instantiate with random UUID as correlation ID")
    void shouldBeAbleToInstantiateWithRandomUUIDAsCorrelationID(){
        var execContext = ExecutionContext.ofNew();
        Assertions.assertNotNull(execContext.getCorrelationId());
        var actor = Mockito.mock(Actor.class);
        var anotherExecContext = ExecutionContext.ofNew(actor);
        Assertions.assertNotNull(anotherExecContext.getCorrelationId());
    }

    @Test
    @DisplayName("Should be instance of GenericExecutionManager")
    void shouldBeInstanceOfGenericExecutionManager(){
        Assertions.assertInstanceOf(GenericExecutionManager.class, ExecutionContext.ofNew());
    }

    @Test
    @DisplayName("Should be able to add step insights as expected")
    void shouldBeAbleToAddStepInsightsAsExpected(){
        var execContext = ExecutionContext.ofNew();
        execContext.setSubjectAndStartTracking("some-test");
        var firstStepName = "first-step";
        var secondStepName = "second-step";
        Assertions.assertTrue(execContext.getStepInsights().isEmpty());
        execContext.addStepInsightsOf(firstStepName);
        Assertions.assertFalse(execContext.getStepInsights().isEmpty());
        Assertions.assertEquals(1, execContext.getStepInsights().size());
        var hasFirstStep = execContext.getStepInsights()
                .stream()
                .map(ExecutionContext.StepInsight::getSubject)
                .anyMatch(subject -> subject.equals(firstStepName));
        Assertions.assertTrue(hasFirstStep);
        execContext.addStepInsightsOf(secondStepName);
        Assertions.assertFalse(execContext.getStepInsights().isEmpty());
        Assertions.assertEquals(2, execContext.getStepInsights().size());
        var hasSecondStep = execContext.getStepInsights()
                .stream()
                .map(ExecutionContext.StepInsight::getSubject)
                .anyMatch(subject -> subject.equals(secondStepName));
        Assertions.assertTrue(hasSecondStep);
    }

    @Test
    @DisplayName("Should throw InternalMappedException if attempted with adding a step before getting started")
    void shouldThrowInternalMappedExceptionIfAttemptedWithAddingAStepBeforeGettingStarted(){
        var execContext = ExecutionContext.ofNew();
        Assertions.assertThrows(InternalMappedException.class, () -> execContext.addStepInsightsOf("ops!"));
    }

    @Test
    @DisplayName("New instances should always have the step list initiated but empty")
    void newInstancesShouldAlwaysHaveTheStepListInitiatedButEmpty(){
        var actor = Mockito.mock(Actor.class);
        var correlationId = UUID.randomUUID();
        ExecutionContext executionContext;
        executionContext = ExecutionContext.ofNew();
        Assertions.assertNotNull(executionContext.getStepInsights());
        Assertions.assertTrue(executionContext.getStepInsights().isEmpty());
        executionContext = ExecutionContext.ofNew(actor);
        Assertions.assertNotNull(executionContext.getStepInsights());
        Assertions.assertTrue(executionContext.getStepInsights().isEmpty());
        executionContext = ExecutionContext.of(correlationId.toString());
        Assertions.assertNotNull(executionContext.getStepInsights());
        Assertions.assertTrue(executionContext.getStepInsights().isEmpty());
        executionContext = ExecutionContext.of(correlationId.toString(), actor);
        Assertions.assertNotNull(executionContext.getStepInsights());
        Assertions.assertTrue(executionContext.getStepInsights().isEmpty());
        executionContext = new ExecutionContext(correlationId);
        Assertions.assertNotNull(executionContext.getStepInsights());
        Assertions.assertTrue(executionContext.getStepInsights().isEmpty());
        executionContext = new ExecutionContext(correlationId, actor);
        Assertions.assertNotNull(executionContext.getStepInsights());
        Assertions.assertTrue(executionContext.getStepInsights().isEmpty());
    }

    @Test
    @DisplayName("The toString method should reflect the correlation ID")
    void theToStringMethodShouldReflectTheCorrelationId(){
        var correlationId = UUID.randomUUID();
        var actor = Mockito.mock(Actor.class);
        var execContext = new ExecutionContext(correlationId, actor);
        Assertions.assertEquals(correlationId.toString(), execContext.toString());
    }

    @Test
    @DisplayName("StepInsight instances should get started from the get-go")
    void stepInsightInstancesShouldGetStartedFromTheGetGo(){
        var newStepInsight = ExecutionContext.StepInsight.of("some-subject-here");
        Assertions.assertTrue(newStepInsight.hasStarted());
    }

    @Test
    @DisplayName("StepInsight instances' toString should be as expected for successful executions")
    void stepInsightInstancesToStringShouldBeAsExpectedForSuccessfulExecutions(){
        var newStepInsight = ExecutionContext.StepInsight.of("some-other-subject-once-again");
        newStepInsight.complete();
        var expectedToStringFormat = newStepInsight.getSubject()
                + "'s insights: ("
                + newStepInsight.calculateLatency().toString() + "ms) "
                + "no exception has been thrown";
        Assertions.assertEquals(expectedToStringFormat, newStepInsight.toString());
    }

    @Test
    @DisplayName("StepInsight instances' toString should be as expected for unsuccessful executions")
    void stepInsightInstancesToStringShouldBeAsExpectedForUnsuccessfulExecutions(){
        var newStepInsight = ExecutionContext.StepInsight.of("one-more-subject-once-again");
        newStepInsight.complete(new Exception("ops..."));
        var expectedToStringFormat = newStepInsight.getSubject()
                + "'s insights: ("
                + newStepInsight.calculateLatency().toString() + "ms) "
                + "an exception has been thrown along the way: " + newStepInsight.getException();
        Assertions.assertEquals(expectedToStringFormat, newStepInsight.toString());
    }

}
