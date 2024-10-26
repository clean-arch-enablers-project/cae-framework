package com.cae.use_cases.specifics.suppliers;

import com.cae.loggers.StackTraceLogger;
import com.cae.trier.Trier;
import com.cae.use_cases.UseCase;
import com.cae.use_cases.auto_logger.AutoLoggingManager;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.exceptions.UseCaseExecutionException;

/**
 * Specific type of UseCase which has no input but provides output
 * @param <O> type of output
 */
public abstract class SupplierUseCase <O> extends UseCase {

    protected SupplierUseCase() {
        super();
    }

    /**
     * Public method which triggers the execution of the SupplierUseCase.
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
     * @param context unique identifier of the use case execution
     * @return the use case execution output
     */
    public O execute(ExecutionContext context){
        return Trier.of(() -> {
            this.handleAuthorization(context);
            return this.finallyExecute(context);
        })
        .setHandlerForUnexpectedException(unexpectedException -> new UseCaseExecutionException(this, unexpectedException))
        .finishAndExecuteAction();
    }

    private O finallyExecute(ExecutionContext context) {
        var loggingManager = AutoLoggingManager.of(this, context);
        try {
            var output = this.applyInternalLogic(context);
            loggingManager.logExecution(context, null, output, null);
            return output;
        } catch (Exception anyException){
            StackTraceLogger.SINGLETON.handleLoggingStackTrace(anyException, context, this.getUseCaseMetadata().getName());
            loggingManager.logExecution(context, null, null, anyException);
            throw anyException;
        }
    }

    /**
     * Internal method supposed to execute the core logic of the use case
     * @param context unique identifier of the use case execution
     * @return the use case execution output
     */
    protected abstract O applyInternalLogic(ExecutionContext context);

}
