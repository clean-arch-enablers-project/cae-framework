package com.cae.use_cases;

import com.cae.autofeatures.autoauth.ResourceOwnershipRetriever;
import com.cae.use_cases.contexts.ExecutionContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.MockedAutofeaturesRunnerProvider;
import utils.normal_use_cases.SomeNormalConsumerUseCase;
import utils.problematic_use_cases.SomeProblematicConsumerUseCaseWithUnexpectedException;

@ExtendWith(MockitoExtension.class)
class ConsumerUseCaseTest {

    private final SomeNormalConsumerUseCase.Input input = new SomeNormalConsumerUseCase.Input();
    private final SomeProblematicConsumerUseCaseWithUnexpectedException.Input inputForProblematicUc = new SomeProblematicConsumerUseCaseWithUnexpectedException.Input();

    @BeforeEach
    void setup(){
        this.input.setFieldOne("some value here");
    }

    @Test
    @DisplayName("Should initialize ExecutionContext when the execute method is called")
    void shouldInitializeExecutionContextWhenTheExecuteMethodIsCalled(){
        MockedAutofeaturesRunnerProvider.run();
        var newExecContext = ExecutionContext.ofNew();
        var startTimeWasNullBeforeExecution = newExecContext.getStartTime() == null;
        var subjectWasNullBeforeExecution = newExecContext.getSubject() == null;
        var unit = new SomeNormalConsumerUseCase();
        unit.execute(this.input, newExecContext);
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
        var unit = new SomeNormalConsumerUseCase();
        unit.execute(this.input, newExecContext);
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
        var unit = new SomeNormalConsumerUseCase();
        Mockito.verify(MockedAutofeaturesRunnerProvider.autologExecutor, Mockito.times(0)).submit(ArgumentMatchers.any(Runnable.class));
        Mockito.verify(MockedAutofeaturesRunnerProvider.autometricsExecutor, Mockito.times(0)).submit(ArgumentMatchers.any(Runnable.class));
        Mockito.verify(MockedAutofeaturesRunnerProvider.autonotifyExecutor, Mockito.times(0)).submit(ArgumentMatchers.any(Runnable.class));
        unit.execute(this.input, newExecContext);
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
        var unit = new SomeNormalConsumerUseCase();
        unit.execute(this.input, newExecContext);
        var isCompleteAfterExecution = newExecContext.getEndTime() != null;
        Assertions.assertTrue(wasIncompleteBeforeExecution);
        Assertions.assertTrue(isCompleteAfterExecution);
        Assertions.assertTrue(newExecContext.wasSuccessful());
        MockedAutofeaturesRunnerProvider.flushMockedInstances();
    }

    @Test
    @DisplayName("Should add the Input data to the ExecutionContext when successfully finishes")
    void shouldAddTheInputDataToTheExecutionContextWhenSuccessfullyFinishes(){
        MockedAutofeaturesRunnerProvider.run();
        var newExecContext = ExecutionContext.ofNew();
        var hadNoInputDataBeforeExecution = newExecContext.getInput() == null;
        var unit = new SomeNormalConsumerUseCase();
        unit.execute(this.input, newExecContext);
        var hasInputDataAfterExecution = newExecContext.getInput() != null;
        Assertions.assertTrue(hadNoInputDataBeforeExecution);
        Assertions.assertTrue(hasInputDataAfterExecution);
        MockedAutofeaturesRunnerProvider.flushMockedInstances();
    }

    @Test
    @DisplayName("Should catch unexpected exception and transform it into a UseCaseExecutionException")
    void shouldCatchUnexpectedExceptionAndTransformItIntoAUseCaseExecutionException(){
        var problematicUseCaseWithUnexpectedException = new SomeProblematicConsumerUseCaseWithUnexpectedException();
        var execContext = ExecutionContext.ofNew();
        try {
            problematicUseCaseWithUnexpectedException.execute(this.inputForProblematicUc, execContext);
        } catch (Exception exception){
            Assertions.assertInstanceOf(UseCaseExecutionException.class, exception);
        }
    }

    @Test
    @DisplayName("Should mark ExecutionContext as unsuccessful when implementation throws unexpected exception")
    void shouldMarkExecutionContextAsUnsuccessfulWhenImplementationThrowsUnexpectedException(){
        var problematicUseCaseWithUnexpectedException = new SomeProblematicConsumerUseCaseWithUnexpectedException();
        var execContext = ExecutionContext.ofNew();
        try {
            problematicUseCaseWithUnexpectedException.execute(this.inputForProblematicUc, execContext);
        } catch (Exception exception){
            Assertions.assertFalse(execContext.wasSuccessful());
            Assertions.assertNotNull(execContext.getException());
            var useCaseExecutionException = (UseCaseExecutionException) exception;
            Assertions.assertTrue(useCaseExecutionException.getOriginalException().isPresent());
            Assertions.assertEquals(useCaseExecutionException.getOriginalException().get(), execContext.getException());
        }
    }

    @Test
    @DisplayName("Should have the resourceOwnershipRetriever null when the default constructor is the one invoked")
    void shouldHaveTheResourceOwnershipRetrieverNullWhenTheDefaultConstructorIsTheOneInvoked(){
        var unit = new SomeNormalConsumerUseCase();
        Assertions.assertNull(unit.resourceOwnershipRetriever);
        Assertions.assertTrue(unit.getResourceOwnershipRetriever().isEmpty());
    }

    @Test
    @DisplayName("Should have the resourceOwnershipRetriever set when provided via constructor")
    void shouldHaveTheResourceOwnershipRetrieverSetWhenProvidedViaConstructor(){
        var retriever = Mockito.mock(ResourceOwnershipRetriever.class);
        var unit = new SomeNormalConsumerUseCase(retriever);
        Assertions.assertEquals(retriever, unit.resourceOwnershipRetriever);
        Assertions.assertTrue(unit.getResourceOwnershipRetriever().isPresent());
        Assertions.assertEquals(retriever, unit.getResourceOwnershipRetriever().get());
    }

}
