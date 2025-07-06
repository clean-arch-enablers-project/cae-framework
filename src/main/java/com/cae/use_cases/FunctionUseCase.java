package com.cae.use_cases;

import com.cae.autofeatures.PostExecutionAutofeaturesRunner;
import com.cae.autofeatures.PreExecutionAutofeaturesRunner;
import com.cae.autofeatures.autoauth.ResourceOwnershipRetriever;
import com.cae.trier.Trier;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.io.UseCaseInput;

import java.util.Optional;

/**
 * Specific type of UseCase which has both input and output
 * @param <I> type of input
 * @param <O> type of output
 */
public abstract class FunctionUseCase <I extends UseCaseInput, O> extends UseCase implements UseCaseWithInput {

    protected FunctionUseCase() {
        super();
        this.resourceOwnershipRetriever = null;
    }

    protected FunctionUseCase(ResourceOwnershipRetriever resourceOwnershipRetriever){
        super();
        this.resourceOwnershipRetriever = resourceOwnershipRetriever;
    }

    protected final ResourceOwnershipRetriever resourceOwnershipRetriever;

    public Optional<ResourceOwnershipRetriever> getResourceOwnershipRetriever(){
        return Optional.ofNullable(this.resourceOwnershipRetriever);
    }

    public O execute(I input, ExecutionContext context){
        return Trier.of(() -> {
            context.setSubjectAndStartTracking(this.getUseCaseMetadata().getName());
            return this.run(input, context);
        })
        .setUnexpectedExceptionHandler(unexpectedException -> new UseCaseExecutionException(this, unexpectedException))
        .execute();
    }

    private O run(I input, ExecutionContext context) {
        try {
            input.autoverify();
            PreExecutionAutofeaturesRunner.run(input, context, this);
            var output = this.applyInternalLogic(input, context);
            context.complete();
            context.setInput(input);
            context.setOutput(output);
            PostExecutionAutofeaturesRunner.runOn(context);
            return output;
        } catch (Exception anyException){
            context.complete(anyException);
            context.setInput(input);
            PostExecutionAutofeaturesRunner.runOn(context);
            throw anyException;
        }
    }

    /**
     * Internal method supposed to execute the core logic of the use case
     * @param input input of the use case
     * @return output of the use case
     */
    protected abstract O applyInternalLogic(I input, ExecutionContext context);

}
