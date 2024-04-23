package com.cae.ports.specifics.suppliers;


import com.cae.ports.Port;
import com.cae.ports.exceptions.PortExecutionException;
import com.cae.trier.Trier;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;

/**
 * Specific type of port: supplier ports are ports that don't have input
 * but supply something as return of their execution.
 * @param <O> the output type
 */
public abstract class SupplierPort <O> extends Port {

    /**
     * Method accessible for triggering the port execution.
     * @param correlation the correlation of the use case execution
     * @return the output expected for the port
     */
    public O executePort(UseCaseExecutionCorrelation correlation){
        var output = Trier.of(this::executeLogic, correlation)
                .setHandlerForUnexpectedException(unexpectedException -> new PortExecutionException(unexpectedException, this.getName()))
                .finishAndExecuteAction();
        this.handleIOLogs(null, output, correlation);
        return output;
    }

    /**
     * This method is supposed to be implemented within the concrete
     * classes that will be supplier ports. It is in this method that the
     * port logic is supposed to be contained.
     * @param correlation the correlation ID being passed by one of
     *                    the public methods
     * @return the output expected for the port execution
     */
    protected abstract O executeLogic(UseCaseExecutionCorrelation correlation);

}
