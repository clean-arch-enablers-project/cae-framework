package com.cae.mapped_exceptions.specifics;

import com.cae.mapped_exceptions.MappedException;

/**
 * More specific type of MappedException thought to cover cases in
 * which something was not found during the execution of
 * your application. Maybe you were checking for the existence of
 * a Company record and ended up finding out it doesn't exist; that
 * would be a perfect example of throwing a CompanyNotFoundException
 * exception which would extend the NotFoundMappedException.
 * That would be similar to, in HTTP, returning a 404 status code.
 */
public class NotFoundMappedException extends MappedException {

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
    public NotFoundMappedException(String briefPublicMessage, String details){
        super(briefPublicMessage, details);
    }

    /**
     * Constructor method for exceptions that are just fine with only a
     * brief and public message, with no need of a more detailed
     * explanation. The 'message' attribute inherited from the
     * RuntimeException will be set only containing the brief public
     * message.
     * @param briefPublicMessage the brief and public info
     */
    public NotFoundMappedException(String briefPublicMessage){
        super(briefPublicMessage);
    }

}
