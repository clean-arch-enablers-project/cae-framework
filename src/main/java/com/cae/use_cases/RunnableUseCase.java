package com.cae.use_cases;

import com.cae.autofeatures.PostExecutionAutofeaturesRunner;
import com.cae.autofeatures.PreExecutionAutofeaturesRunner;
import com.cae.trier.Trier;
import com.cae.use_cases.contexts.ExecutionContext;

/**
 * Specific type of UseCase which has neither input nor output
 */
public abstract class RunnableUseCase extends UseCase {

    protected RunnableUseCase() {
        super();
    }

    public void execute(ExecutionContext context){
        Trier.of(() -> {
            context.setSubjectAndStartTracking(this.getUseCaseMetadata().getName());
            this.run(context);
        })
        .setUnexpectedExceptionHandler(unexpectedException -> new UseCaseExecutionException(this, unexpectedException))
        .execute();
    }

    private void run(ExecutionContext context) {
        try {
            PreExecutionAutofeaturesRunner.run(context, this);
            this.applyInternalLogic(context);
            context.complete();
            PostExecutionAutofeaturesRunner.runOn(context);
        } catch (Exception anyException){
            context.complete(anyException);
            PostExecutionAutofeaturesRunner.runOn(context);
            throw anyException;
        }
    }

    protected abstract void applyInternalLogic(ExecutionContext context);

}
