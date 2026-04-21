package utils.normal_use_cases;

import com.cae.context.ExecutionContext;
import com.cae.framework.use_cases.SupplierUseCase;
import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

public class SomeNormalSupplierUseCase extends SupplierUseCase<SomeNormalSupplierUseCase.Output> {


    @Getter
    @Setter
    public static class Output{
        private UUID someId;
    }

    @Override
    protected Output applyInternalLogic(ExecutionContext context) {
        var output = new Output();
        output.setSomeId(UUID.randomUUID());
        return output;
    }
}
