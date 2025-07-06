package com.cae.ports;

import com.cae.ports.exceptions.PortExecutionException;
import com.cae.trier.Trier;
import com.cae.use_cases.contexts.ExecutionContext;

/**
 * Specific type of port: runnable ports are ports that have neither
 * input nor output.
 */
public abstract class RunnablePort extends Port {

    public void executePort(ExecutionContext context){
        Trier.of(() -> {
            var stepInsights = context.addStepInsightsOf(this.getName());
            try {
                this.executeLogic(context);
                stepInsights.complete();
            } catch (Exception anyException){
                stepInsights.complete(anyException);
                throw anyException;
            }
        })
        .setUnexpectedExceptionHandler(unexpectedException -> new PortExecutionException(unexpectedException, this.name))
        .execute();
    }

    protected abstract void executeLogic(ExecutionContext correlation);

}
