package com.cae.use_cases;

import com.cae.autofeatures.PostExecutionAutofeaturesRunner;
import com.cae.autofeatures.PreExecutionAutofeaturesRunner;
import com.cae.trier.Trier;
import com.cae.use_cases.contexts.ExecutionContext;

/**
 * Represents a specific type of {@link UseCase} that does not require input but produces an output.
 * <p>
 * This type of use case is useful for retrieving system-level information, generating values,
 * or running background tasks where no external parameters are needed.
 * </p>
 *
 * @param <O> the type of output produced by the use case
 */
public abstract class SupplierUseCase <O> extends UseCase {

    /**
     * Creates a new {@code SupplierUseCase} instance.
     */
    protected SupplierUseCase() {
        super();
    }

    /**
     * Executes the use case using the given {@link ExecutionContext} and returns its output.
     * <p>
     * This method delegates execution through the {@link Trier} mechanism, which provides
     * exception handling, retry logic, and lifecycle hooks. If any exception is thrown during its execution,
     * it will check whether the exception is a {@code MappedException}. If so, it will rethrow it as it is interpreted as
     * fruit of your own design.
     * If not, it will wrap it inside a {@link UseCaseExecutionException} and throw it.
     * It also triggers pre- and post-execution Autofeatures automatically.
     * </p>
     *
     * @param context the current execution context
     * @return the output of the use case
     * @throws UseCaseExecutionException if an unexpected exception occurs during execution
     */
    public O execute(ExecutionContext context){
        return Trier.of(() -> {
            context.setSubjectAndStartTracking(this.getUseCaseMetadata().getName(), true);
            return this.run(context);
        })
        .onUnexpectedExceptions(unexpectedException -> new UseCaseExecutionException(this, unexpectedException))
        .execute();
    }

    /**
     * Internal execution pipeline that manages the use case lifecycle.
     * <p>
     * This includes:
     * <ul>
     *     <li>Running pre-execution Autofeatures,</li>
     *     <li>Invoking business logic via {@link #applyInternalLogic},</li>
     *     <li>Completing the execution context and registering the output,</li>
     *     <li>Running post-execution Autofeatures,</li>
     *     <li>Capturing and recording exceptions if any are thrown.</li>
     * </ul>
     *
     * @param context the current execution context
     * @return the output produced by the use case
     */
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

    /**
     * The actual business logic that should be implemented by subclasses of this use case.
     * <p>
     * This method will be executed after pre-execution autofeatures such as autoauth and autoverify and
     * before post-execution ones such as autolog, autometrics and autonotify.
     * Any exception thrown will be captured, and the execution context will be marked accordingly.
     * </p>
     *
     * @param context the active execution context
     * @return output produced by the use case
     */
    protected abstract O applyInternalLogic(ExecutionContext context);

}
