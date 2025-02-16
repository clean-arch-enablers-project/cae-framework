package com.cae.use_cases;

import com.cae.autolog.StackTraceLogger;
import com.cae.autonotify.Autonotify;
import com.cae.trier.Trier;
import com.cae.autolog.Autolog;
import com.cae.use_cases.contexts.ExecutionContext;

import java.time.Duration;
import java.time.LocalDateTime;

/**
 * Specific type of UseCase which has neither input nor output
 */
public abstract class RunnableUseCase extends UseCase {

    protected RunnableUseCase() {
        super();
    }

    /**
     * Public method which triggers the execution of the RunnableUseCase.
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
     * @param context the unique identifier of the use case execution
     */
    public void execute(ExecutionContext context){
        Trier.of(() -> {
            this.handleScopeBasedAuthorization(context);
            this.finallyExecute(context);
        })
        .setUnexpectedExceptionHandler(unexpectedException -> new UseCaseExecutionException(this, unexpectedException))
        .execute();
    }

    private void finallyExecute(ExecutionContext context) {
        var loggingManager = Autolog.of(this, context);
        var startingMoment = LocalDateTime.now();
        try {
            this.applyInternalLogic(context);
            var latency = Duration.between(startingMoment, LocalDateTime.now()).toMillis();
            Autonotify.handleNotificationOn(this, null, latency, context);
            loggingManager.logExecution(context, null, null, null, latency);
        } catch (Exception anyException){
            var latency = Duration.between(startingMoment, LocalDateTime.now()).toMillis();
            Autonotify.handleNotificationOn(this, anyException, latency, context);
            StackTraceLogger.SINGLETON.handleLoggingStackTrace(anyException, context, this.getUseCaseMetadata().getName());
            loggingManager.logExecution(context, null, null, anyException, latency);
            throw anyException;
        }
    }

    /**
     * Internal method supposed to execute the core logic of the use case
     * @param context the unique identifier of the use case execution
     */
    protected abstract void applyInternalLogic(ExecutionContext context);

}
