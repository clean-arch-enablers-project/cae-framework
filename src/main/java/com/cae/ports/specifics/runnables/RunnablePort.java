package com.cae.ports.specifics.runnables;

import com.cae.ports.Port;
import com.cae.ports.auto_logging.PortInsightsManager;
import com.cae.ports.exceptions.PortExecutionException;
import com.cae.trier.Trier;
import com.cae.use_cases.contexts.ExecutionContext;

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
            try {
                this.executeLogic(context);
                insightsManager.keepInsightOf(context, null, null, null);
            } catch (Exception anyException){
                insightsManager.keepInsightOf(context, null, null, anyException);
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
