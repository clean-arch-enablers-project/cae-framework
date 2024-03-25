package com.cae.use_cases.specifics.consumers;

import com.cae.mapped_exceptions.MappedException;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import com.cae.use_cases.exceptions.UseCaseExecutionException;
import com.cae.use_cases.io.UseCaseInput;
import com.cae.use_cases.metadata.UseCaseMetadata;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.LoggerForTesting;

@ExtendWith(MockitoExtension.class)
class ConsumerUseCaseTest {

    private final UseCaseExecutionCorrelation correlation = UseCaseExecutionCorrelation.ofNew();

    @Test
    void shouldCallTheValidatePropertiesMethodFromInput(){
        var input = Mockito.mock(TheConsumerUseCaseImplementationInput.class);
        Mockito.doNothing().when(input).validateProperties();
        var useCase = new SomeConsumerUseCaseImplementation();
        useCase.execute(input, this.correlation);
        Mockito.verify(input, Mockito.times(1)).validateProperties();
    }

    @Test
    void shouldRunWithoutProblemsTheUseCase(){
        var input = Mockito.mock(TheConsumerUseCaseImplementationInput.class);
        Mockito.doNothing().when(input).validateProperties();
        var useCase = new SomeConsumerUseCaseImplementation();
        Assertions.assertDoesNotThrow(() -> useCase.execute(input, this.correlation));
    }

    @Test
    void shouldHandleNotExpectedExceptionDuringUseCaseExecution(){
        var input = Mockito.mock(TheConsumerUseCaseImplementationInput.class);
        Mockito.doNothing().when(input).validateProperties();
        var useCase = new SomeUnexpectedProblematicConsumerUseCaseImplementation();
        Assertions.assertThrows(UseCaseExecutionException.class, () -> useCase.execute(input, this.correlation));
    }

    @Test
    void shouldHandleExpectedExceptionDuringUseCaseExecution(){
        var input = Mockito.mock(TheConsumerUseCaseImplementationInput.class);
        Mockito.doNothing().when(input).validateProperties();
        var useCase = new SomeExpectedProblematicConsumerUseCaseImplementation();
        Assertions.assertThrows(SomeExpectedShitThatMightHappen.class, () -> useCase.execute(input, this.correlation));
    }

    private static class SomeConsumerUseCaseImplementation extends ConsumerUseCase<TheConsumerUseCaseImplementationInput>{
        protected SomeConsumerUseCaseImplementation() {
            super(UseCaseMetadata.ofOpenAccessUseCase(SomeConsumerUseCaseImplementation.class, "Just for testing"), LoggerForTesting.getSingletonInstance());
        }
        @Override
        protected void applyInternalLogic(TheConsumerUseCaseImplementationInput input, UseCaseExecutionCorrelation correlation) {
            this.logger.logInfo("Just initialized my internal logic");
        }
    }

    private static class SomeUnexpectedProblematicConsumerUseCaseImplementation extends ConsumerUseCase<TheConsumerUseCaseImplementationInput>{
        protected SomeUnexpectedProblematicConsumerUseCaseImplementation() {
            super(UseCaseMetadata.ofOpenAccessUseCase(SomeConsumerUseCaseImplementation.class, "Just for testing"), LoggerForTesting.getSingletonInstance());
        }
        @Override
        protected void applyInternalLogic(TheConsumerUseCaseImplementationInput input, UseCaseExecutionCorrelation correlation) {
            throw new RuntimeException("some unexpected internal error");
        }
    }

    private static class SomeExpectedProblematicConsumerUseCaseImplementation extends ConsumerUseCase<TheConsumerUseCaseImplementationInput>{
        protected SomeExpectedProblematicConsumerUseCaseImplementation() {
            super(UseCaseMetadata.ofOpenAccessUseCase(SomeConsumerUseCaseImplementation.class, "Just for testing"), LoggerForTesting.getSingletonInstance());
        }
        @Override
        protected void applyInternalLogic(TheConsumerUseCaseImplementationInput input, UseCaseExecutionCorrelation correlation) {
            throw new SomeExpectedShitThatMightHappen();
        }
    }

    private static class TheConsumerUseCaseImplementationInput extends UseCaseInput {
    }

    public static class SomeExpectedShitThatMightHappen extends MappedException {

        public SomeExpectedShitThatMightHappen() {
            super("some expected internal error that could happen");
        }
    }


}
