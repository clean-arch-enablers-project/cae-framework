package com.cae.ports;

import com.cae.ports.exceptions.PortExecutionException;
import com.cae.trier.Trier;
import com.cae.use_cases.contexts.ExecutionContext;

/**
 * Specific type of port: function ports are ports that both receive
 * inputs and return outputs.
 * @param <I> the input type
 * @param <O> the output type
 */
public abstract class FunctionPort <I, O> extends Port {

    public O executePortOn(I input, ExecutionContext context){
        return Trier.of(() -> {
            var stepInsights = context.addStepInsightsOf(this.getName());
            stepInsights.setInput(input);
            try {
                var output = this.executeLogic(input, context);
                stepInsights.complete();
                stepInsights.setOutput(output);
                return output;
            } catch (Exception anyException){
                stepInsights.complete(anyException);
                throw anyException;
            }
        })
        .setUnexpectedExceptionHandler(unexpectedException -> new PortExecutionException(unexpectedException, this.name))
        .execute();
    }

    protected abstract O executeLogic(I input, ExecutionContext correlation);

}
