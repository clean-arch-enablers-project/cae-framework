package com.cae.mapped_exceptions.specifics;

import com.cae.mapped_exceptions.MappedException;

/**
 * More specific type of MappedException thought to cover cases in
 * which something went wrong internally during the execution of
 * your application. Maybe some remote call received an unexpected
 * status code and your application ain't prepared for handling it,
 * maybe some implementation that is dispatching your use case
 * called your primary port passing an object that doesn't meet your
 * requirements, so you check it and throw an
 * InternalMappedException, which would be similar to, in HTTP,
 * returning a 5xx status code.
 */
public class InternalMappedException extends MappedException {

    /**
     * Constructor method for exceptions that have both a brief,
     * public message and more details about its cause. The 'message'
     * attribute inherited from the RuntimeException will be set
     * concatenating the brief public message with the detailed info.
     * @param briefPublicMessage the public and brief message, which
     *                           is supposed to be more friendly and accessible to
     *                           the outside world (such as in the return of a REST
     *                           endpoint, for instance).
     * @param details the more detailed info about the cause of the
     *                exception. That info is supposed to be used in internal
     *                affairs (such as in logging controls).
     */
    public InternalMappedException(String briefPublicMessage, String details){
        super(briefPublicMessage, details);
    }

}
