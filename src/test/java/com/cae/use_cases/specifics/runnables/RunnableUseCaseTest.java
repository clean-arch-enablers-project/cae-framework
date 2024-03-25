package com.cae.use_cases.specifics.runnables;

import com.cae.mapped_exceptions.MappedException;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import com.cae.use_cases.exceptions.UseCaseExecutionException;
import com.cae.use_cases.metadata.UseCaseMetadata;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.LoggerForTesting;

@ExtendWith(MockitoExtension.class)
class RunnableUseCaseTest {

    private final UseCaseExecutionCorrelation correlation = UseCaseExecutionCorrelation.ofNew();

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
            super(UseCaseMetadata.ofOpenAccessUseCase(SomeNormalRunnableUseCaseImplementation.class, "Just for testing"), LoggerForTesting.getSingletonInstance());
        }
        @Override
        protected void applyInternalLogic(UseCaseExecutionCorrelation correlation) {
            this.logger.logInfo("Just executed my internal logic");
        }
    }

    private static class SomeUnexpectedProblematicRunnableUseCaseImplementation extends RunnableUseCase{
        protected SomeUnexpectedProblematicRunnableUseCaseImplementation() {
            super(UseCaseMetadata.ofOpenAccessUseCase(SomeUnexpectedProblematicRunnableUseCaseImplementation.class, "Just for testing"), LoggerForTesting.getSingletonInstance());
        }
        @Override
        protected void applyInternalLogic(UseCaseExecutionCorrelation correlation) {
            throw new RuntimeException("some unexpected internal error");
        }
    }

    private static class SomeExpectedProblematicRunnableUseCaseImplementation extends RunnableUseCase{
        protected SomeExpectedProblematicRunnableUseCaseImplementation() {
            super(UseCaseMetadata.ofOpenAccessUseCase(SomeExpectedProblematicRunnableUseCaseImplementation.class, "Just for testing"), LoggerForTesting.getSingletonInstance());
        }
        @Override
        protected void applyInternalLogic(UseCaseExecutionCorrelation correlation) {
            throw new SomeExpectedShitThatMightHappen();
        }
    }

    public static class SomeExpectedShitThatMightHappen extends MappedException {

        public SomeExpectedShitThatMightHappen() {
            super("some expected internal error that could happen");
        }
    }


}
