package utils.normal_use_cases;

import com.cae.autofeatures.autoauth.ResourceOwnershipRetriever;
import com.cae.use_cases.ConsumerUseCase;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.io.UseCaseInput;
import com.cae.use_cases.io.annotations.NotNullInputField;
import lombok.Getter;
import lombok.Setter;

public class SomeNormalConsumerUseCase extends ConsumerUseCase<SomeNormalConsumerUseCase.Input> {

    public SomeNormalConsumerUseCase(){
        super();
    }

    public SomeNormalConsumerUseCase(ResourceOwnershipRetriever retriever) {
        super(retriever);
    }

    @Getter
    @Setter
    public static class Input extends UseCaseInput {
        @NotNullInputField
        private String fieldOne;
    }

    @Override
    protected void applyInternalLogic(Input input, ExecutionContext context) {
        System.out.println("Just some Consumer UC");
    }
}
