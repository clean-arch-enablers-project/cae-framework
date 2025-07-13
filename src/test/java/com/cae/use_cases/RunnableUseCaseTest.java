package com.cae.use_cases;

import com.cae.use_cases.contexts.ExecutionContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.MockedAutofeaturesRunnerProvider;
import utils.normal_use_cases.SomeNormalRunnableUseCase;
import utils.problematic_use_cases.SomeProblematicRunnableUseCaseWithUnexpectedException;

@ExtendWith(MockitoExtension.class)
class RunnableUseCaseTest {

    @Test
    @DisplayName("Should initialize ExecutionContext when the execute method is called")
    void shouldInitializeExecutionContextWhenTheExecuteMethodIsCalled(){
        MockedAutofeaturesRunnerProvider.run();
        var newExecContext = ExecutionContext.ofNew();
        var startTimeWasNullBeforeExecution = newExecContext.getStartTime() == null;
        var subjectWasNullBeforeExecution = newExecContext.getSubject() == null;
        var unit = new SomeNormalRunnableUseCase();
        unit.execute(newExecContext);
        var startTimeIsFilledAfterExecution = newExecContext.getEndTime() != null;
        var subjectIsFilledAfterExecution = newExecContext.getSubject() != null;
        Assertions.assertTrue(startTimeWasNullBeforeExecution);
        Assertions.assertTrue(subjectWasNullBeforeExecution);
        Assertions.assertTrue(startTimeIsFilledAfterExecution);
        Assertions.assertTrue(subjectIsFilledAfterExecution);
        MockedAutofeaturesRunnerProvider.flushMockedInstances();
    }

    @Test
    @DisplayName("Should set the use case name as the ExecutionContext subject")
    void shouldSetTheUseCaseNameAsTheExecutionContextSubject(){
        MockedAutofeaturesRunnerProvider.run();
        var newExecContext = ExecutionContext.ofNew();
        var subjectNameBeforeActualExecution = newExecContext.getSubject();
        var unit = new SomeNormalRunnableUseCase();
        unit.execute(newExecContext);
        var useCaseName = unit.getUseCaseMetadata().getName();
        var subjectNameAfterActualExecution = newExecContext.getSubject();
        Assertions.assertNull(subjectNameBeforeActualExecution);
        Assertions.assertEquals(useCaseName, subjectNameAfterActualExecution);
        MockedAutofeaturesRunnerProvider.flushMockedInstances();
    }

    @Test
    @DisplayName("Should call all the Post-Execution Autofeature Executors")
    void shouldCallAllThePostExecutionAutofeatureExecutors(){
        MockedAutofeaturesRunnerProvider.run();
        var newExecContext = ExecutionContext.ofNew();
        var unit = new SomeNormalRunnableUseCase();
        Mockito.verify(MockedAutofeaturesRunnerProvider.autologExecutor, Mockito.times(0)).submit(ArgumentMatchers.any(Runnable.class));
        Mockito.verify(MockedAutofeaturesRunnerProvider.autometricsExecutor, Mockito.times(0)).submit(ArgumentMatchers.any(Runnable.class));
        Mockito.verify(MockedAutofeaturesRunnerProvider.autonotifyExecutor, Mockito.times(0)).submit(ArgumentMatchers.any(Runnable.class));
        unit.execute(newExecContext);
        Mockito.verify(MockedAutofeaturesRunnerProvider.autologExecutor, Mockito.times(1)).submit(ArgumentMatchers.any(Runnable.class));
        Mockito.verify(MockedAutofeaturesRunnerProvider.autometricsExecutor, Mockito.times(1)).submit(ArgumentMatchers.any(Runnable.class));
        Mockito.verify(MockedAutofeaturesRunnerProvider.autonotifyExecutor, Mockito.times(1)).submit(ArgumentMatchers.any(Runnable.class));
        MockedAutofeaturesRunnerProvider.flushMockedInstances();
    }

    @Test
    @DisplayName("Should mark exec context as complete when execution successfully finishes")
    void shouldMarExecContextAsCompleteWhenExecutionSuccessfullyFinishes(){
        MockedAutofeaturesRunnerProvider.run();
        var newExecContext = ExecutionContext.ofNew();
        var wasIncompleteBeforeExecution = newExecContext.getEndTime() == null;
        var unit = new SomeNormalRunnableUseCase();
        unit.execute(newExecContext);
        var isCompleteAfterExecution = newExecContext.getEndTime() != null;
        Assertions.assertTrue(wasIncompleteBeforeExecution);
        Assertions.assertTrue(isCompleteAfterExecution);
        Assertions.assertTrue(newExecContext.wasSuccessful());
        MockedAutofeaturesRunnerProvider.flushMockedInstances();
    }

    @Test
    @DisplayName("Should catch unexpected exception and transform it into a UseCaseExecutionException")
    void shouldCatchUnexpectedExceptionAndTransformItIntoAUseCaseExecutionException(){
        var problematicUseCaseWithUnexpectedException = new SomeProblematicRunnableUseCaseWithUnexpectedException();
        var execContext = ExecutionContext.ofNew();
        try {
            problematicUseCaseWithUnexpectedException.execute(execContext);
        } catch (Exception exception){
            Assertions.assertInstanceOf(UseCaseExecutionException.class, exception);
        }
    }

    @Test
    @DisplayName("Should mark ExecutionContext as unsuccessful when implementation throws unexpected exception")
    void shouldMarkExecutionContextAsUnsuccessfulWhenImplementationThrowsUnexpectedException(){
        var problematicUseCaseWithUnexpectedException = new SomeProblematicRunnableUseCaseWithUnexpectedException();
        var execContext = ExecutionContext.ofNew();
        try {
            problematicUseCaseWithUnexpectedException.execute(execContext);
        } catch (Exception exception){
            Assertions.assertFalse(execContext.wasSuccessful());
            Assertions.assertNotNull(execContext.getException());
            var useCaseExecutionException = (UseCaseExecutionException) exception;
            Assertions.assertTrue(useCaseExecutionException.getOriginalException().isPresent());
            Assertions.assertEquals(useCaseExecutionException.getOriginalException().get(), execContext.getException());
        }
    }

}
