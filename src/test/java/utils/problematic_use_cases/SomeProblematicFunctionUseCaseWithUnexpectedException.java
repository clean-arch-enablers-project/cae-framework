package utils.problematic_use_cases;

import com.cae.use_cases.FunctionUseCase;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.io.UseCaseInput;

public class SomeProblematicFunctionUseCaseWithUnexpectedException extends
        FunctionUseCase<
            SomeProblematicFunctionUseCaseWithUnexpectedException.Input,
            SomeProblematicFunctionUseCaseWithUnexpectedException.Output
        > {

    public static class Input extends UseCaseInput{}

    public static class Output{}

    @Override
    protected Output applyInternalLogic(Input input, ExecutionContext context) {
        throw new RuntimeException("ops...");
    }

}
