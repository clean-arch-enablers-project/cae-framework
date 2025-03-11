package com.cae.use_cases;

import com.cae.mapped_exceptions.MappedException;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.io.UseCaseInput;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.simulations.assemblers.loggers.MyAppAutologBootstrap;

@ExtendWith(MockitoExtension.class)
class ConsumerUseCaseTest {

    private final ExecutionContext correlation = ExecutionContext.ofNew();

    @BeforeEach
    void setup(){
        MyAppAutologBootstrap.startupDefaultSettings();
    }

    @Test
    void shouldCallTheValidatePropertiesMethodFromInput(){
        var input = Mockito.mock(TheConsumerUseCaseImplementationInput.class);
        Mockito.doNothing().when(input).autoverify();
        var useCase = new SomeConsumerUseCaseImplementation();
        useCase.execute(input, this.correlation);
        Mockito.verify(input, Mockito.times(1)).autoverify();
    }

    @Test
    void shouldRunWithoutProblemsTheUseCase(){
        var input = Mockito.mock(TheConsumerUseCaseImplementationInput.class);
        Mockito.doNothing().when(input).autoverify();
        var useCase = new SomeConsumerUseCaseImplementation();
        Assertions.assertDoesNotThrow(() -> useCase.execute(input, this.correlation));
    }

    @Test
    void shouldHandleNotExpectedExceptionDuringUseCaseExecution(){
        var input = Mockito.mock(TheConsumerUseCaseImplementationInput.class);
        Mockito.doNothing().when(input).autoverify();
        var useCase = new SomeUnexpectedProblematicConsumerUseCaseImplementation();
        Assertions.assertThrows(UseCaseExecutionException.class, () -> useCase.execute(input, this.correlation));
    }

    @Test
    void shouldHandleExpectedExceptionDuringUseCaseExecution(){
        var input = Mockito.mock(TheConsumerUseCaseImplementationInput.class);
        Mockito.doNothing().when(input).autoverify();
        var useCase = new SomeExpectedProblematicConsumerUseCaseImplementation();
        Assertions.assertThrows(SomeExpectedShitThatMightHappen.class, () -> useCase.execute(input, this.correlation));
    }

    private static class SomeConsumerUseCaseImplementation extends ConsumerUseCase<TheConsumerUseCaseImplementationInput> {
        @Override
        protected void applyInternalLogic(TheConsumerUseCaseImplementationInput input, ExecutionContext context) {
            this.getLogger().logInfo("Just initialized my internal logic");
        }
    }

    private static class SomeUnexpectedProblematicConsumerUseCaseImplementation extends ConsumerUseCase<TheConsumerUseCaseImplementationInput>{
        @Override
        protected void applyInternalLogic(TheConsumerUseCaseImplementationInput input, ExecutionContext context) {
            throw new RuntimeException("some unexpected internal error");
        }
    }

    private static class SomeExpectedProblematicConsumerUseCaseImplementation extends ConsumerUseCase<TheConsumerUseCaseImplementationInput>{
        @Override
        protected void applyInternalLogic(TheConsumerUseCaseImplementationInput input, ExecutionContext context) {
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
