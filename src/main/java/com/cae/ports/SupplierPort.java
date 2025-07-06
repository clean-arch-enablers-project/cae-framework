package com.cae.ports;


import com.cae.ports.exceptions.PortExecutionException;
import com.cae.trier.Trier;
import com.cae.use_cases.contexts.ExecutionContext;

/**
 * Specific type of port: supplier ports are ports that don't have input
 * but supply something as return of their execution.
 * @param <O> the output type
 */
public abstract class SupplierPort <O> extends Port {

    public O executePort(ExecutionContext context){
        return Trier.of(() -> {
            var stepInsights = context.addStepInsightsOf(this.getName());
            try {
                var output = this.executeLogic(context);
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

    protected abstract O executeLogic(ExecutionContext correlation);

}
