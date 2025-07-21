package com.cae.use_cases;

import com.cae.autofeatures.PostExecutionAutofeaturesRunner;
import com.cae.autofeatures.PreExecutionAutofeaturesRunner;
import com.cae.autofeatures.autoauth.ResourceOwnershipRetriever;
import com.cae.trier.Trier;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.io.UseCaseInput;

import java.util.Optional;

/**
 * Represents a specific type of {@link UseCase} that accepts input and produces output.
 * <p>
 * This type of use case is suitable for query-style operations or any business logic
 * that must return a result to the caller, such as calculations, data retrieval,
 * or transformations.
 * </p>
 *
 * @param <I> the input type, which must implement {@link UseCaseInput}
 * @param <O> the output type returned by the use case
 */
public abstract class FunctionUseCase <I extends UseCaseInput, O> extends UseCase implements UseCaseWithInput {

    /**
     * Creates a {@code FunctionUseCase} without an associated {@link ResourceOwnershipRetriever}.
     * Typically used for public or unrestricted use cases or when using scope-based protection or role-based protection but
     * the {@code UseCaseInput} has a {@code @ResourceOwnerIdentifier} annotated
     * field instead of a {@code @ResourceIdentifier} one.
     */
    protected FunctionUseCase() {
        super();
        this.resourceOwnershipRetriever = null;
    }

    /**
     * Creates a {@code FunctionUseCase} with an associated {@link ResourceOwnershipRetriever}.
     * Used to enforce ownership validation before execution for role-based protected instances that use a
     * {@code @ResourceIdentifier} annotated field instead of a {@code @ResourceOwnerIdentifier} one.
     *
     * @param resourceOwnershipRetriever the ownership retriever to associate with this use case
     */
    protected FunctionUseCase(ResourceOwnershipRetriever resourceOwnershipRetriever){
        super();
        this.resourceOwnershipRetriever = resourceOwnershipRetriever;
    }

    protected final ResourceOwnershipRetriever resourceOwnershipRetriever;

    /**
     * Retrieves the optional {@link ResourceOwnershipRetriever} associated with this use case.
     * If present, it can be used to retrieve the owner of the resource which has its identifier under the
     * {@code UseCaseInput} field annotated with {@code @ResourceIdentifier}.
     *
     * @return an {@code Optional} containing the {@code ResourceOwnershipRetriever}, if any
     */
    public Optional<ResourceOwnershipRetriever> getResourceOwnershipRetriever(){
        return Optional.ofNullable(this.resourceOwnershipRetriever);
    }

    /**
     * Executes the use case with the given input and execution context.
     * <p>
     * This method delegates execution through the {@link Trier} mechanism, which provides
     * exception handling, retry logic, and lifecycle hooks. If any exception is thrown during its execution,
     * it will check whether the exception is a {@code MappedException}. If so, it will rethrow it as it is interpreted as
     * fruit of your own design.
     * If not, it will wrap it inside a {@link UseCaseExecutionException} and throw it.
     * It also triggers pre- and post-execution Autofeatures automatically.
     * </p>
     *
     * @param input the input for the use case
     * @param context the execution context that carries correlation data and shared objects
     * @return the result produced by the use case
     * @throws UseCaseExecutionException if an unexpected exception occurs during execution
     */
    public O execute(I input, ExecutionContext context){
        return Trier.of(() -> {
            context.setSubjectAndStartTracking(this.getUseCaseMetadata().getName(), true);
            return this.run(input, context);
        })
        .onUnexpectedExceptions(unexpectedException -> new UseCaseExecutionException(this, unexpectedException))
        .execute();
    }

    /**
     * Internal execution pipeline for the use case.
     * <p>
     * This method is responsible for:
     * <ul>
     *     <li>Verifying the input contract via {@code autoverify},</li>
     *     <li>Running any registered pre-execution Autofeatures,</li>
     *     <li>Executing the business logic via {@link #applyInternalLogic},</li>
     *     <li>Completing the context and triggering post-execution Autofeatures,</li>
     *     <li>Tracking exceptions and attaching them to the context if any occur.</li>
     * </ul>
     * </p>
     *
     * @param input the use case input
     * @param context the current execution context
     * @return the result produced by the use case
     */
    private O run(I input, ExecutionContext context) {
        try {
            input.autoverify(context);
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
