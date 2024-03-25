package com.cae.use_cases.specifics.suppliers;

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
class SupplierUseCaseTest {

    private final UseCaseExecutionCorrelation correlation = UseCaseExecutionCorrelation.ofNew();

    @Test
    void shouldCallTheValidatePropertiesMethodFromInput(){
        var useCase = new SomeNormalSupplierUseCaseImplementation();
        var useCaseResult = useCase.execute(this.correlation);
        Assertions.assertNotNull(useCaseResult);
        Assertions.assertEquals("Just executed my internal logic", useCaseResult);
    }

    @Test
    void shouldRunWithoutProblemsTheUseCase(){
        var useCase = new SomeNormalSupplierUseCaseImplementation();
        Assertions.assertDoesNotThrow(() -> useCase.execute(this.correlation));
    }

    @Test
    void shouldHandleNotExpectedExceptionDuringUseCaseExecution(){
        var useCase = new SomeUnexpectedProblematicSupplierUseCaseImplementation();
        Assertions.assertThrows(UseCaseExecutionException.class, () -> useCase.execute(this.correlation));
    }

    @Test
    void shouldHandleExpectedExceptionDuringUseCaseExecution(){
        var useCase = new SomeExpectedProblematicSupplierUseCaseImplementation();
        Assertions.assertThrows(SomeExpectedShitThatMightHappen.class, () -> useCase.execute(this.correlation));
    }

    private static class SomeNormalSupplierUseCaseImplementation extends SupplierUseCase<String> {
        protected SomeNormalSupplierUseCaseImplementation() {
            super(UseCaseMetadata.ofOpenAccessUseCase(SomeNormalSupplierUseCaseImplementation.class, "Just for testing"), LoggerForTesting.getSingletonInstance());
        }
        @Override
        protected String applyInternalLogic(UseCaseExecutionCorrelation correlation) {
            return ("Just executed my internal logic");
        }
    }

    private static class SomeUnexpectedProblematicSupplierUseCaseImplementation extends SupplierUseCase<String>{
        protected SomeUnexpectedProblematicSupplierUseCaseImplementation() {
            super(UseCaseMetadata.ofOpenAccessUseCase(SomeUnexpectedProblematicSupplierUseCaseImplementation.class, "Just for testing"), LoggerForTesting.getSingletonInstance());
        }
        @Override
        protected String applyInternalLogic(UseCaseExecutionCorrelation correlation) {
            throw new RuntimeException("some unexpected internal error");
        }
    }

    private static class SomeExpectedProblematicSupplierUseCaseImplementation extends SupplierUseCase<String>{
        protected SomeExpectedProblematicSupplierUseCaseImplementation() {
            super(UseCaseMetadata.ofOpenAccessUseCase(SomeExpectedProblematicSupplierUseCaseImplementation.class, "Just for testing"), LoggerForTesting.getSingletonInstance());
        }
        @Override
        protected String applyInternalLogic(UseCaseExecutionCorrelation correlation) {
            throw new SomeExpectedShitThatMightHappen();
        }
    }

    public static class SomeExpectedShitThatMightHappen extends MappedException {

        public SomeExpectedShitThatMightHappen() {
            super("some expected internal error that could happen");
        }
    }


}
