package utils.problematic_use_cases;

import com.cae.context.ExecutionContext;
import com.cae.framework.use_cases.FunctionUseCase;
import com.cae.framework.use_cases.io.UseCaseInput;

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
