package com.cae.framework.ports;

import com.cae.context.ExecutionContext;
import com.cae.framework.ports.exceptions.PortExecutionException;
import com.cae.trier.Trier;

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
        .onUnexpectedExceptions(unexpectedException -> new PortExecutionException(unexpectedException, this.name))
        .execute();
    }

    protected abstract void executeLogic(ExecutionContext correlation);

}
