package utils;

import com.cae.autofeatures.autoauth.ResourceOwnershipRetriever;
import com.cae.use_cases.FunctionUseCase;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.io.UseCaseInput;
import com.cae.use_cases.io.annotations.NotNullInputField;
import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

public class SomeNormalFunctionUseCase extends FunctionUseCase<SomeNormalFunctionUseCase.Input, SomeNormalFunctionUseCase.Output> {

    public SomeNormalFunctionUseCase(){
        super();
    }

    public SomeNormalFunctionUseCase(ResourceOwnershipRetriever retriever) {
        super(retriever);
    }

    @Getter
    @Setter
    public static class Input extends UseCaseInput {
        @NotNullInputField
        private String fieldOne;
    }

    @Getter
    @Setter
    public static class Output{
        private UUID someId;
    }

    @Override
    protected Output applyInternalLogic(Input input, ExecutionContext context) {
        var output = new Output();
        output.setSomeId(UUID.randomUUID());
        return output;
    }
}
