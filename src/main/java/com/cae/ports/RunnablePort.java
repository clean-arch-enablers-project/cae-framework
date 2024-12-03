package com.cae.ports;

import com.cae.loggers.StackTraceLogger;
import com.cae.notifier.NotifierManager;
import com.cae.ports.autolog.PortInsightsManager;
import com.cae.ports.exceptions.PortExecutionException;
import com.cae.trier.Trier;
import com.cae.use_cases.contexts.ExecutionContext;

import java.time.Duration;
import java.time.LocalDateTime;

/**
 * Specific type of port: runnable ports are ports that have neither
 * input nor output.
 */
public abstract class RunnablePort extends Port {

    /**
     * Method accessible for triggering the port execution.
     * @param context the context of the use case execution
     */
    public void executePort(ExecutionContext context){
        Trier.of(() -> {
            var insightsManager = PortInsightsManager.of(this.name);
            var startingMoment = LocalDateTime.now();
            try {
                this.executeLogic(context);
                var latency = Duration.between(startingMoment, LocalDateTime.now()).toMillis();
                NotifierManager.handleNotificationOn(this, null, latency, context);
                insightsManager.keepInsightOf(context, null, null, null, latency);
            } catch (Exception anyException){
                var latency = Duration.between(startingMoment, LocalDateTime.now()).toMillis();
                NotifierManager.handleNotificationOn(this, anyException, latency, context);
                StackTraceLogger.SINGLETON.handleLoggingStackTrace(anyException, context, this.name);
                insightsManager.keepInsightOf(context, null, null, anyException, latency);
                throw anyException;
            }
        })
        .setHandlerForUnexpectedException(unexpectedException -> new PortExecutionException(unexpectedException, this.name))
        .finishAndExecuteAction();
    }

    /**
     * This method is supposed to be implemented within the concrete
     * classes that will be runnable ports. It is in this method that the
     * port logic is supposed to be contained.
     * @param correlation the correlation ID being passed by one of
     *                    the public methods
     */
    protected abstract void executeLogic(ExecutionContext correlation);

}
