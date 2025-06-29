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
 * Specific type of port: function ports are ports that both receive
 * inputs and return outputs.
 * @param <I> the input type
 * @param <O> the output type
 */
public abstract class FunctionPort <I, O> extends Port {

    /**
     * Method accessible for triggering the port execution.
     * @param input its input
     * @param context the context of the use case execution
     * @return the expected output of the port
     */
    public O executePortOn(I input, ExecutionContext context){
        return Trier.of(() -> {
            var insightsManager = PortInsightsManager.of(this.name);
            var startingMoment = LocalDateTime.now();
            try {
                var output = this.executeLogic(input, context);
                var latency = Duration.between(startingMoment, LocalDateTime.now()).toMillis();
                Autonotify.handleNotificationOn(this, null, latency, context);
                Autometrics.collect(Metric.of(this.getName(), latency, null, false));
                insightsManager.keepInsightOf(context, input, output, null, latency);
                return output;
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
     * classes that will be function ports. It is in this method that the
     * port logic is supposed to be contained.
     * @param input the input to process being passed by one of the
     *              public methods
     * @param correlation the correlation ID being passed by one of
     *                    the public methods
     * @return the expected output of the port
     */
    protected abstract O executeLogic(I input, ExecutionContext correlation);

}
