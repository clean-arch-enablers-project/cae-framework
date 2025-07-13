package com.cae.use_cases;

import com.cae.autofeatures.PostExecutionAutofeaturesRunner;
import com.cae.autofeatures.PreExecutionAutofeaturesRunner;
import com.cae.trier.Trier;
import com.cae.use_cases.contexts.ExecutionContext;

/**
 * Represents a specific type of {@link UseCase} that requires no input and produces no output.
 * <p>
 * This type is best suited for self-contained operations such as scheduled jobs, initialization
 * routines, cache refreshes, or other internal tasks where no external data is needed or returned.
 * </p>
 */
public abstract class RunnableUseCase extends UseCase {

    /**
     * Creates a new {@code RunnableUseCase} instance.
     */
    protected RunnableUseCase() {
        super();
    }

    /**
     * Executes the use case with the given {@link ExecutionContext}.
     * <p>
     * This method delegates execution through the {@link Trier} mechanism, which provides
     * exception handling, retry logic, and lifecycle hooks. If any exception is thrown during its execution,
     * it will check whether the exception is a {@code MappedException}. If so, it will rethrow it as it is interpreted as
     * fruit of your own design.
     * If not, it will wrap it inside a {@link UseCaseExecutionException} and throw it.
     * It also triggers pre- and post-execution Autofeatures automatically.
     * </p>
     *
     * @param context the context representing the current execution environment
     * @throws UseCaseExecutionException if an unexpected exception occurs during execution
     */
    public void execute(ExecutionContext context){
        Trier.of(() -> {
            context.setSubjectAndStartTracking(this.getUseCaseMetadata().getName());
            this.run(context);
        })
        .onUnexpectedExceptions(unexpectedException -> new UseCaseExecutionException(this, unexpectedException))
        .execute();
    }

    /**
     * Internal execution pipeline for the use case.
     * <p>
     * Handles execution lifecycle concerns such as:
     * <ul>
     *     <li>Pre-execution Autofeatures,</li>
     *     <li>Business logic execution,</li>
     *     <li>Context completion and tracking,</li>
     *     <li>Post-execution Autofeatures.</li>
     * </ul>
     * Exceptions are captured and registered into the context before being rethrown.
     *
     * @param context the current execution context
     */
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

    /**
     * The core business logic to be implemented by subclasses.
     * <p>
     * This method should contain the actual behavior of the use case.
     * It will be invoked after the execution context is fully prepared and pre-execution hooks have run.
     * </p>
     *
     * @param context the current execution context
     */
    protected abstract void applyInternalLogic(ExecutionContext context);

}
