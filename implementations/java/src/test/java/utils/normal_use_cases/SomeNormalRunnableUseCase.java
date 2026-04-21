package utils.normal_use_cases;

import com.cae.context.ExecutionContext;
import com.cae.framework.use_cases.RunnableUseCase;
import com.cae.framework.use_cases.io.UseCaseInput;
import com.cae.framework.use_cases.io.annotations.NotNullInputField;
import lombok.Getter;
import lombok.Setter;

public class SomeNormalRunnableUseCase extends RunnableUseCase {

    @Getter
    @Setter
    public static class Input extends UseCaseInput {
        @NotNullInputField
        private String fieldOne;
    }

    @Override
    protected void applyInternalLogic(ExecutionContext context) {
        System.out.println("Just some Runnable UC");
    }
}
