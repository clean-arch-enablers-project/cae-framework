package com.cae.trier;


import com.cae.mapped_exceptions.MappedException;

/**
 * FunctionalInterface for handlers of unexpected exceptions being
 * caught (exceptions that don't extend any kind of MappedException).
 */
@FunctionalInterface
public interface UnexpectedExceptionHandler {

    /**
     * Contract of handling the unexpected exception: receives it and
     * returns out of it a new, mapped-typed, exception instance.
     * @param unexpectedException the exception that does not extend
     *                            any kind of MappedException
     * @return the MappedException as a result of handling the unexpected
     */
    MappedException handle(Exception unexpectedException);

}
