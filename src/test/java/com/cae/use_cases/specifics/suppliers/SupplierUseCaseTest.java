package com.cae.use_cases.specifics.suppliers;

import com.cae.mapped_exceptions.MappedException;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.exceptions.UseCaseExecutionException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.simulations.assemblers.loggers.LoggerBootstrapForTesting;

@ExtendWith(MockitoExtension.class)
class SupplierUseCaseTest {

    private final ExecutionContext correlation = ExecutionContext.ofNew();

    @BeforeEach
    void setup(){
        LoggerBootstrapForTesting.startupDefaultSettings();
    }

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
        @Override
        protected String applyInternalLogic(ExecutionContext context) {
            return ("Just executed my internal logic");
        }
    }

    private static class SomeUnexpectedProblematicSupplierUseCaseImplementation extends SupplierUseCase<String>{
        @Override
        protected String applyInternalLogic(ExecutionContext context) {
            throw new RuntimeException("some unexpected internal error");
        }
    }

    private static class SomeExpectedProblematicSupplierUseCaseImplementation extends SupplierUseCase<String>{
        @Override
        protected String applyInternalLogic(ExecutionContext context) {
            throw new SomeExpectedShitThatMightHappen();
        }
    }

    public static class SomeExpectedShitThatMightHappen extends MappedException {

        public SomeExpectedShitThatMightHappen() {
            super("some expected internal error that could happen");
        }
    }


}
