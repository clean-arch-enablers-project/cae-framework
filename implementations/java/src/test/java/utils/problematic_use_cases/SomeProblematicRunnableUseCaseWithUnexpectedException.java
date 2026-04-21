package utils.problematic_use_cases;

import com.cae.context.ExecutionContext;
import com.cae.framework.use_cases.RunnableUseCase;

public class SomeProblematicRunnableUseCaseWithUnexpectedException extends RunnableUseCase {

    @Override
    protected void applyInternalLogic(ExecutionContext context) {
        throw new RuntimeException("ops...");
    }

}
