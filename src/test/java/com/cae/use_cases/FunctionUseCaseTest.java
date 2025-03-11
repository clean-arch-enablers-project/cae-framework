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
class FunctionUseCaseTest {

    private final ExecutionContext correlation = ExecutionContext.ofNew();

    @BeforeEach
    void setup(){
        MyAppAutologBootstrap.startupDefaultSettings();
    }

    @Test
    void shouldCallTheValidatePropertiesMethodFromInput(){
        var input = Mockito.mock(TheFunctionUseCaseImplementationInput.class);
        Mockito.doNothing().when(input).autoverify();
        var useCase = new SomeNormalFunctionUseCaseImplementation();
        var useCaseResult = useCase.execute(input, this.correlation);
        Assertions.assertNotNull(useCaseResult);
        Assertions.assertEquals("Just executed my internal logic", useCaseResult);
        Mockito.verify(input, Mockito.times(1)).autoverify();
    }

    @Test
    void shouldRunWithoutProblemsTheUseCase(){
        var input = Mockito.mock(TheFunctionUseCaseImplementationInput.class);
        Mockito.doNothing().when(input).autoverify();
        var useCase = new SomeNormalFunctionUseCaseImplementation();
        Assertions.assertDoesNotThrow(() -> useCase.execute(input, this.correlation));
    }

    @Test
    void shouldHandleNotExpectedExceptionDuringUseCaseExecution(){
        var input = Mockito.mock(TheFunctionUseCaseImplementationInput.class);
        Mockito.doNothing().when(input).autoverify();
        var useCase = new SomeUnexpectedProblematicFunctionUseCaseImplementation();
        Assertions.assertThrows(UseCaseExecutionException.class, () -> useCase.execute(input, this.correlation));
    }

    @Test
    void shouldHandleExpectedExceptionDuringUseCaseExecution(){
        var input = Mockito.mock(TheFunctionUseCaseImplementationInput.class);
        Mockito.doNothing().when(input).autoverify();
        var useCase = new SomeExpectedProblematicFunctionUseCaseImplementation();
        Assertions.assertThrows(SomeExpectedShitThatMightHappen.class, () -> useCase.execute(input, this.correlation));
    }

    private static class SomeNormalFunctionUseCaseImplementation extends FunctionUseCase<TheFunctionUseCaseImplementationInput, String> {
        @Override
        protected String applyInternalLogic(TheFunctionUseCaseImplementationInput input, ExecutionContext context) {
            return ("Just executed my internal logic");
        }
    }

    private static class SomeUnexpectedProblematicFunctionUseCaseImplementation extends FunctionUseCase<TheFunctionUseCaseImplementationInput, String>{
        @Override
        protected String applyInternalLogic(TheFunctionUseCaseImplementationInput input, ExecutionContext context) {
            throw new RuntimeException("some unexpected internal error");
        }
    }

    private static class SomeExpectedProblematicFunctionUseCaseImplementation extends FunctionUseCase<TheFunctionUseCaseImplementationInput, String>{
        @Override
        protected String applyInternalLogic(TheFunctionUseCaseImplementationInput input, ExecutionContext context) {
            throw new SomeExpectedShitThatMightHappen();
        }
    }

    private static class TheFunctionUseCaseImplementationInput extends UseCaseInput {
    }

    public static class SomeExpectedShitThatMightHappen extends MappedException {

        public SomeExpectedShitThatMightHappen() {
            super("some expected internal error that could happen");
        }
    }


}
