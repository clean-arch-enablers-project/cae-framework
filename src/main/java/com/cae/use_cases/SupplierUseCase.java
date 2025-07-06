package com.cae.use_cases;

import com.cae.autofeatures.PostExecutionAutofeaturesRunner;
import com.cae.autofeatures.PreExecutionAutofeaturesRunner;
import com.cae.trier.Trier;
import com.cae.use_cases.contexts.ExecutionContext;

/**
 * Specific type of UseCase which has no input but provides output
 * @param <O> type of output
 */
public abstract class SupplierUseCase <O> extends UseCase {

    protected SupplierUseCase() {
        super();
    }

    public O execute(ExecutionContext context){
        return Trier.of(() -> {
            context.setSubjectAndStartTracking(this.getUseCaseMetadata().getName());
            return this.run(context);
        })
        .setUnexpectedExceptionHandler(unexpectedException -> new UseCaseExecutionException(this, unexpectedException))
        .execute();
    }

    private O run(ExecutionContext context) {
        try {
            PreExecutionAutofeaturesRunner.run(context, this);
            var output = this.applyInternalLogic(context);
            context.complete();
            context.setOutput(output);
            PostExecutionAutofeaturesRunner.runOn(context);
            return output;
        } catch (Exception anyException){
            context.complete(anyException);
            PostExecutionAutofeaturesRunner.runOn(context);
            throw anyException;
        }
    }

    protected abstract O applyInternalLogic(ExecutionContext context);

}
