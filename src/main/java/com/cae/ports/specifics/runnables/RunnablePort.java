package com.cae.ports.specifics.runnables;

import com.cae.ports.Port;
import com.cae.ports.exceptions.PortExecutionException;
import com.cae.trier.Trier;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;

/**
 * Specific type of port: runnable ports are ports that have neither
 * input nor output.
 */
public abstract class RunnablePort extends Port {

    /**
     * Method accessible for triggering the port execution.
     * @param correlation the correlation of the use case execution
     */
    public void executePort(UseCaseExecutionCorrelation correlation){
        Trier.of(this::executeLogic, correlation)
                .setHandlerForUnexpectedException(unexpectedException -> new PortExecutionException(unexpectedException, this.getName()))
                .finishAndExecuteAction();
    }

    /**
     * This method is supposed to be implemented within the concrete
     * classes that will be runnable ports. It is in this method that the
     * port logic is supposed to be contained.
     * @param correlation the correlation ID being passed by one of
     *                    the public methods
     */
    protected abstract void executeLogic(UseCaseExecutionCorrelation correlation);

}
