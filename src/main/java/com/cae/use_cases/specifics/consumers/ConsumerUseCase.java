package com.cae.use_cases.specifics.consumers;

import com.cae.trier.Trier;
import com.cae.use_cases.UseCase;
import com.cae.use_cases.auto_logger.AutoLoggingManager;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.exceptions.UseCaseExecutionException;
import com.cae.use_cases.io.UseCaseInput;

/**
 * Specific type of UseCase which only has input but no output.
 * @param <I> type of input
 */
public abstract class ConsumerUseCase <I extends UseCaseInput> extends UseCase {

    protected ConsumerUseCase() {
        super();
    }

    /**
     * Public method which triggers the execution of the ConsumerUseCase.
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
     */
    public void execute(I input, ExecutionContext context){
        Trier.of(() -> {
            this.handleAuthorization(context);
            input.validateProperties();
            this.finallyExecute(input, context);
        })
        .setHandlerForUnexpectedException(unexpectedException -> new UseCaseExecutionException(this, unexpectedException))
        .finishAndExecuteAction();
    }

    private void finallyExecute(I input, ExecutionContext context) {
        var loggingManager = AutoLoggingManager.of(this, context);
        try {
            this.applyInternalLogic(input, context);
            loggingManager.logExecution(context, input, null, null);
        } catch (Exception anyException){
            loggingManager.logExecution(context, input, null, anyException);
            throw anyException;
        }
    }

    /**
     * Internal method supposed to execute the core logic of the use case
     * @param input input of the use case
     */
    protected abstract void applyInternalLogic(I input, ExecutionContext context);


}
