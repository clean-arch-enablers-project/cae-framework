package utils.problematic_use_cases;

import com.cae.use_cases.ConsumerUseCase;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.io.UseCaseInput;

public class SomeProblematicConsumerUseCaseWithUnexpectedException extends
        ConsumerUseCase<SomeProblematicConsumerUseCaseWithUnexpectedException.Input> {

    public static class Input extends UseCaseInput{}

    @Override
    protected void applyInternalLogic(Input input, ExecutionContext context) {
        throw new RuntimeException("ops...");
    }

}
