package utils.problematic_use_cases;

import com.cae.use_cases.SupplierUseCase;
import com.cae.use_cases.contexts.ExecutionContext;

public class SomeProblematicSupplierUseCaseWithUnexpectedException extends SupplierUseCase<SomeProblematicSupplierUseCaseWithUnexpectedException.Output> {

    public static class Output{}

    @Override
    protected Output applyInternalLogic(ExecutionContext context) {
        throw new RuntimeException("ops...");
    }

}
