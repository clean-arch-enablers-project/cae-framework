package utils.problematic_use_cases;

import com.cae.use_cases.RunnableUseCase;
import com.cae.use_cases.contexts.ExecutionContext;

public class SomeProblematicRunnableUseCaseWithUnexpectedException extends RunnableUseCase {

    @Override
    protected void applyInternalLogic(ExecutionContext context) {
        throw new RuntimeException("ops...");
    }

}
