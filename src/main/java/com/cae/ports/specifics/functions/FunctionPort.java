package com.cae.ports.specifics.functions;

import com.cae.ports.Port;
import com.cae.ports.auto_logging.PortInsightsManager;
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

    /**
     * Method accessible for triggering the port execution.
     * @param input its input
     * @param context the context of the use case execution
     * @return the expected output of the port
     */
    public O executePortOn(I input, ExecutionContext context){
        return Trier.of(() -> {
            var insightsManager = PortInsightsManager.of(this.name);
            try {
                var output = this.executeLogic(input, context);
                insightsManager.keepInsightOf(context, input, output, null);
                return output;
            } catch (Exception anyException){
                insightsManager.keepInsightOf(context, input, null, anyException);
                throw anyException;
            }
        })
        .setHandlerForUnexpectedException(unexpectedException -> new PortExecutionException(unexpectedException, this.name))
        .finishAndExecuteAction();
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
