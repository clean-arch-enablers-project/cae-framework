package com.cae.use_cases;

import com.cae.autofeatures.PostExecutionAutofeaturesRunner;
import com.cae.autofeatures.PreExecutionAutofeaturesRunner;
import com.cae.autofeatures.autoauth.ResourceOwnershipRetriever;
import com.cae.trier.Trier;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.io.UseCaseInput;

import java.util.Optional;

/**
 * Specific type of UseCase which only has input but no output.
 * @param <I> type of input
 */
public abstract class ConsumerUseCase <I extends UseCaseInput> extends UseCase implements UseCaseWithInput {

    protected ConsumerUseCase() {
        super();
        this.resourceOwnershipRetriever = null;
    }

    protected ConsumerUseCase(ResourceOwnershipRetriever resourceOwnershipRetriever){
        super();
        this.resourceOwnershipRetriever = resourceOwnershipRetriever;
    }

    protected final ResourceOwnershipRetriever resourceOwnershipRetriever;

    public Optional<ResourceOwnershipRetriever> getResourceOwnershipRetriever(){
        return Optional.ofNullable(this.resourceOwnershipRetriever);
    }

    public void execute(I input, ExecutionContext context){
        Trier.of(() -> {
            context.setSubjectAndStartTracking(this.getUseCaseMetadata().getName());
            this.run(input, context);
        })
        .setUnexpectedExceptionHandler(unexpectedException -> new UseCaseExecutionException(this, unexpectedException))
        .execute();
    }

    private void run(I input, ExecutionContext context) {
        try {
            input.autoverify();
            PreExecutionAutofeaturesRunner.run(input, context, this);
            this.applyInternalLogic(input, context);
            context.complete();
            context.setInput(input);
            PostExecutionAutofeaturesRunner.runOn(context);
        } catch (Exception anyException){
            context.complete(anyException);
            context.setInput(input);
            PostExecutionAutofeaturesRunner.runOn(context);
            throw anyException;
        }
    }

    protected abstract void applyInternalLogic(I input, ExecutionContext context);


}
