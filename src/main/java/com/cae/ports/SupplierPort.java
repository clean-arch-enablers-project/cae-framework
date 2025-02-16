package com.cae.ports;


import com.cae.autolog.StackTraceLogger;
import com.cae.autonotify.Autonotify;
import com.cae.ports.autolog.PortInsightsManager;
import com.cae.ports.exceptions.PortExecutionException;
import com.cae.trier.Trier;
import com.cae.use_cases.contexts.ExecutionContext;

import java.time.Duration;
import java.time.LocalDateTime;

/**
 * Specific type of port: supplier ports are ports that don't have input
 * but supply something as return of their execution.
 * @param <O> the output type
 */
public abstract class SupplierPort <O> extends Port {

    /**
     * Method accessible for triggering the port execution.
     * @param context the context of the use case execution
     * @return the output expected for the port
     */
    public O executePort(ExecutionContext context){
        return Trier.of(() -> {
            var insightsManager = PortInsightsManager.of(this.name);
            var startingMoment = LocalDateTime.now();
            try {
                var output = this.executeLogic(context);
                var latency = Duration.between(startingMoment, LocalDateTime.now()).toMillis();
                Autonotify.handleNotificationOn(this, null, latency, context);
                insightsManager.keepInsightOf(context, null, output, null, latency);
                return output;
            } catch (Exception anyException){
                var latency = Duration.between(startingMoment, LocalDateTime.now()).toMillis();
                Autonotify.handleNotificationOn(this, anyException, latency, context);
                StackTraceLogger.SINGLETON.handleLoggingStackTrace(anyException, context, this.name);
                insightsManager.keepInsightOf(context, null, null, anyException, latency);
                throw anyException;
            }
        })
        .setUnexpectedExceptionHandler(unexpectedException -> new PortExecutionException(unexpectedException, this.name))
        .execute();
    }

    /**
     * This method is supposed to be implemented within the concrete
     * classes that will be supplier ports. It is in this method that the
     * port logic is supposed to be contained.
     * @param correlation the correlation ID being passed by one of
     *                    the public methods
     * @return the output expected for the port execution
     */
    protected abstract O executeLogic(ExecutionContext correlation);

}
