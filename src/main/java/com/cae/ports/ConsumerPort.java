package com.cae.ports;


import com.cae.ports.exceptions.PortExecutionException;
import com.cae.trier.Trier;
import com.cae.use_cases.contexts.ExecutionContext;

/**
 * Specific type of port: consumer ports are ports that receive inputs
 * but don't return anything.
 * @param <I> input type
 */
public abstract class ConsumerPort <I> extends Port {

    public void executePortOn(I input, ExecutionContext context){
        Trier.of(() -> {
            var stepInsights = context.addStepInsightsOf(this.getName());
            stepInsights.setInput(input);
            try {
                this.executeLogic(input, context);
                stepInsights.complete();
            } catch (Exception anyException){
                stepInsights.complete(anyException);
                throw anyException;
            }
        })
        .onUnexpectedExceptions(unexpectedException -> new PortExecutionException(unexpectedException, this.name))
        .execute();
    }

    protected abstract void executeLogic(I input, ExecutionContext correlation);

}
