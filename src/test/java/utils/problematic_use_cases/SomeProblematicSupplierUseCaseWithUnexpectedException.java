package utils.problematic_use_cases;

import com.cae.context.ExecutionContext;
import com.cae.framework.use_cases.SupplierUseCase;

public class SomeProblematicSupplierUseCaseWithUnexpectedException extends SupplierUseCase<SomeProblematicSupplierUseCaseWithUnexpectedException.Output> {

    public static class Output{}

    @Override
    protected Output applyInternalLogic(ExecutionContext context) {
        throw new RuntimeException("ops...");
    }

}
