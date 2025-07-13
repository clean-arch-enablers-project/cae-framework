package com.cae.use_cases;

import com.cae.autofeatures.PostExecutionAutofeaturesRunner;
import com.cae.autofeatures.PreExecutionAutofeaturesRunner;
import com.cae.autofeatures.autoauth.ResourceOwnershipRetriever;
import com.cae.trier.Trier;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.io.UseCaseInput;

import java.util.Optional;

/**
 * Represents a specific type of {@link UseCase} that accepts input but does not return output.
 * <p>
 * Commonly used for command-style operations such as creation, update, deletion, or dispatch logic,
 * where the success or failure of the execution is enough, and no explicit output is needed.
 * </p>
 *
 * @param <I> the type of input accepted by this Use Case, which must implement {@link UseCaseInput}
 */
public abstract class ConsumerUseCase <I extends UseCaseInput> extends UseCase implements UseCaseWithInput {

    /**
     * Creates a {@code ConsumerUseCase} without an associated {@link ResourceOwnershipRetriever}.
     * Typically used for public or unrestricted use cases or when using scope-based protection or role-based protection but
     * the {@code UseCaseInput} has a {@code @ResourceOwnerIdentifier} annotated
     * field instead of a {@code @ResourceIdentifier} one.
     */
    protected ConsumerUseCase() {
        super();
        this.resourceOwnershipRetriever = null;
    }

    /**
     * Creates a {@code ConsumerUseCase} with an associated {@link ResourceOwnershipRetriever}.
     * Used to enforce ownership validation before execution for role-based protected instances that use a
     * {@code @ResourceIdentifier} annotated field instead of a {@code @ResourceOwnerIdentifier} one.
     *
     * @param resourceOwnershipRetriever the ownership retriever to associate with this use case
     */
    protected ConsumerUseCase(ResourceOwnershipRetriever resourceOwnershipRetriever){
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
     * @throws UseCaseExecutionException if an unexpected exception occurs during execution
     */
    public void execute(I input, ExecutionContext context){
        Trier.of(() -> {
            context.setSubjectAndStartTracking(this.getUseCaseMetadata().getName());
            this.run(input, context);
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
     */
    private void run(I input, ExecutionContext context) {
        try {
            input.autoverify(context);
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

    /**
     * The actual business logic that should be implemented by subclasses of this use case.
     * <p>
     * This method will be executed after pre-execution autofeatures such as autoauth and autoverify and
     * before post-execution ones such as autolog, autometrics and autonotify.
     * Any exception thrown will be captured, and the execution context will be marked accordingly.
     * </p>
     *
     * @param input the validated input object
     * @param context the active execution context
     */
    protected abstract void applyInternalLogic(I input, ExecutionContext context);


}
