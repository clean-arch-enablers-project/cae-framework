package utils.problematic_use_cases;

import com.cae.context.ExecutionContext;
import com.cae.framework.use_cases.ConsumerUseCase;
import com.cae.framework.use_cases.io.UseCaseInput;

public class SomeProblematicConsumerUseCaseWithUnexpectedException extends
        ConsumerUseCase<SomeProblematicConsumerUseCaseWithUnexpectedException.Input> {

    public static class Input extends UseCaseInput{}

    @Override
    protected void applyInternalLogic(Input input, ExecutionContext context) {
        throw new RuntimeException("ops...");
    }

}
