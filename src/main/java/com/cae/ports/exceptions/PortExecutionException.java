package com.cae.ports.exceptions;


import com.cae.mapped_exceptions.specifics.InternalMappedException;

/**
 * Any unexpected exception during the execution of Ports will be
 * converted to PortExecutionException instances. As that kind of
 * situation is considered an internal error, its exception representation
 * inherits the InternalMappedException.
 */
public class PortExecutionException extends InternalMappedException {

    /**
     * Constructor the receives the original unexpected exception and
     * the name of the port from which the unexpected exception was
     * thrown.
     * @param unexpectedException the original unexpected exception
     * @param name the port name
     */
    public PortExecutionException(Exception unexpectedException, String name) {
        super("Something went unexpectedly wrong while trying to execute port '" + name + "'", "More details: " + unexpectedException);
    }
}
