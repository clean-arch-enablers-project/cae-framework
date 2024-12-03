package com.cae.use_cases;

import com.cae.mapped_exceptions.MappedException;
import com.cae.use_cases.contexts.ExecutionContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.simulations.assemblers.loggers.LoggerBootstrapForTesting;

@ExtendWith(MockitoExtension.class)
class RunnableUseCaseTest {

    private final ExecutionContext correlation = ExecutionContext.ofNew();

    @BeforeEach
    void setup(){
        LoggerBootstrapForTesting.startupDefaultSettings();
    }

    @Test
    void shouldRunWithoutProblemsTheUseCase(){
        var useCase = new SomeNormalRunnableUseCaseImplementation();
        Assertions.assertDoesNotThrow(() -> useCase.execute(this.correlation));
    }

    @Test
    void shouldHandleNotExpectedExceptionDuringUseCaseExecution(){
        var useCase = new SomeUnexpectedProblematicRunnableUseCaseImplementation();
        Assertions.assertThrows(UseCaseExecutionException.class, () -> useCase.execute(this.correlation));
    }

    @Test
    void shouldHandleExpectedExceptionDuringUseCaseExecution(){
        var useCase = new SomeExpectedProblematicRunnableUseCaseImplementation();
        Assertions.assertThrows(SomeExpectedShitThatMightHappen.class, () -> useCase.execute(this.correlation));
    }

    private static class SomeNormalRunnableUseCaseImplementation extends RunnableUseCase {
        protected SomeNormalRunnableUseCaseImplementation() {
        }
        @Override
        protected void applyInternalLogic(ExecutionContext context) {
            this.getLogger().logInfo("Just executed my internal logic");
        }
    }

    private static class SomeUnexpectedProblematicRunnableUseCaseImplementation extends RunnableUseCase{
        @Override
        protected void applyInternalLogic(ExecutionContext context) {
            throw new RuntimeException("some unexpected internal error");
        }
    }

    private static class SomeExpectedProblematicRunnableUseCaseImplementation extends RunnableUseCase{
        @Override
        protected void applyInternalLogic(ExecutionContext context) {
            throw new SomeExpectedShitThatMightHappen();
        }
    }

    public static class SomeExpectedShitThatMightHappen extends MappedException {

        public SomeExpectedShitThatMightHappen() {
            super("some expected internal error that could happen");
        }
    }


}
