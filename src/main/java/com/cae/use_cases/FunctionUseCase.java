package com.cae.use_cases;

import com.cae.loggers.StackTraceLogger;
import com.cae.trier.Trier;
import com.cae.use_cases.autoauth.ResourceOwnershipRetriever;
import com.cae.use_cases.autoauth.RoleBasedAuth;
import com.cae.use_cases.autolog.AutoLoggingManager;
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

    /**
     * Public method which triggers the execution of the FunctionUseCase.
     * It will internally call the method which keeps the core logic of the
     * use case. If anything goes unexpectedly wrong during its execution,
     * it will throw a UseCaseExecutionException instance with the description
     * of what have gone wrong. If your use case implementation throws
     * a MappedException instance, it will not intercede as it will consider
     * the MappedException as part of the planned flow.
     * Executing your use case with this method assures there will be
     * automated log tracking control: the beginning and the ending of the
     * use case execution will be logged, weather it ends successfully or not.
     * However, you are still free to use your logger instance as you wish
     * inside your use case implementations.
     * @param input the input of the use case
     * @return the output of the use case
     */
    public O execute(I input, ExecutionContext context){
        return Trier.of(() -> {
            this.handleAuthorization(input, context);
            input.validateProperties();
            return this.finallyExecute(input, context);
        })
        .setHandlerForUnexpectedException(unexpectedException -> new UseCaseExecutionException(this, unexpectedException))
        .finishAndExecuteAction();
    }

    private void handleAuthorization(I input, ExecutionContext context) {
        this.handleScopeBasedAuthorization(context);
        RoleBasedAuth.handle(input, context, this);
    }

    private O finallyExecute(I input, ExecutionContext context) {
        var loggingManager = AutoLoggingManager.of(this, context);
        try {
            var output = this.applyInternalLogic(input, context);
            loggingManager.logExecution(context, input, output, null);
            return output;
        } catch (Exception anyException){
            StackTraceLogger.SINGLETON.handleLoggingStackTrace(anyException, context, this.getUseCaseMetadata().getName());
            loggingManager.logExecution(context, input, null, anyException);
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
