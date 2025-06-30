package com.cae.ports;


import com.cae.autofeatures.autolog.StackTraceLogger;
import com.cae.autofeatures.autometrics.Autometrics;
import com.cae.autofeatures.autometrics.Metric;
import com.cae.autofeatures.autonotify.Autonotify;
import com.cae.ports.autolog.PortInsightsManager;
import com.cae.ports.exceptions.PortExecutionException;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.trier.Trier;

import java.time.Duration;
import java.time.LocalDateTime;

/**
 * Specific type of port: consumer ports are ports that receive inputs
 * but don't return anything.
 * @param <I> input type
 */
public abstract class ConsumerPort <I> extends Port {

    /**
     * Method accessible for triggering the port execution.
     * @param input its input object
     * @param context the context of the use case execution
     */
    public void executePortOn(I input, ExecutionContext context){
        Trier.of(() -> {
            var insightsManager = PortInsightsManager.of(this.name);
            var startingMoment = LocalDateTime.now();
            try {
                this.executeLogic(input, context);
                var latency = Duration.between(startingMoment, LocalDateTime.now()).toMillis();
                Autonotify.handleNotificationOn(this, null, latency, context);
                Autometrics.collect(Metric.of(this.getName(), latency, null, false));
                insightsManager.keepInsightOf(context, input, null, null, latency);
            } catch (Exception anyException){
                var latency = Duration.between(startingMoment, LocalDateTime.now()).toMillis();
                Autonotify.handleNotificationOn(this, anyException, latency, context);
                Autometrics.collect(Metric.of(this.getName(), latency, anyException, false));
                StackTraceLogger.SINGLETON.handleLoggingStackTrace(anyException, context, this.name);
                insightsManager.keepInsightOf(context, input, null, anyException, latency);
                throw anyException;
            }
        })
        .setUnexpectedExceptionHandler(unexpectedException -> new PortExecutionException(unexpectedException, this.name))
        .execute();
    }

    /**
     * This method is supposed to be implemented within the concrete
     * classes that will be consumer ports. It is in this method that the
     * port logic is supposed to be contained.
     * @param input the input to process being passed by one of the
     *              public methods
     * @param correlation the correlation ID being passed by one of
     *              the public methods
     */
    protected abstract void executeLogic(I input, ExecutionContext correlation);

}
