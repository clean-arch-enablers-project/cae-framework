package com.cae.ports.specifics.functions;

import com.cae.ports.Port;
import com.cae.ports.exceptions.PortExecutionException;
import com.cae.trier.Trier;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;

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
     * @param correlation the correlation of the use case execution
     * @return the expected output of the port
     */
    public O executePortOn(I input, UseCaseExecutionCorrelation correlation){
        return this.handle(Trier.of(() -> this.executeLogic(input, correlation)));
    }

    private O handle(Trier.TrierBuilder<Void, O> trierBuilder){
        return trierBuilder .setHandlerForUnexpectedException(unexpectedException -> new PortExecutionException(unexpectedException, this.getName()))
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
    protected abstract O executeLogic(I input, UseCaseExecutionCorrelation correlation);

}
