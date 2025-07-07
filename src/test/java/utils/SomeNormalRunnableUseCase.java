package utils;

import com.cae.use_cases.RunnableUseCase;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.io.UseCaseInput;
import com.cae.use_cases.io.annotations.NotNullInputField;
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
