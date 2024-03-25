package com.cae.mapped_exceptions;

import java.util.Optional;

/**
 * Any exception which does not extend MappedException being
 * thrown during a use case execution will be treated as an unexpected
 * exception, as in some kind of unexpected error. On the other hand,
 * any exception which extends MappedException will be treated as
 * exceptions that are part of the workflow of your use case, that means
 * use case processors won't intercept them as they will consider it
 * part of your planned flow. However, if an exception which does
 * not extend MappedException is thrown during a use case execution,
 * the use case processors will catch it and handle it as an unexpected
 * error during your use case flow, breaking the flow.
 * <p></p>
 * For instance, consider your use case checks if a user exists before
 * performing some action: if the user exists, you perform it and if it
 * doesn't, you throw an exception such as 'UserNotFoundException'.
 * That exception would be a perfect example of MappedException, as
 * it is some expected behavior within your use case execution.
 */
public class MappedException extends RuntimeException{

    /**
     * Brief public message for the exception. If you want to only
     * provide some superficial info about the exception, you set this
     * attribute with that kind of data. The more detailed info is supposed
     * to be found, optionally, in the 'details' field.
     */
    protected final String briefPublicMessage;

    /**
     * Details for the understanding of the exception. In some cases we
     * need to, based on an exception, return some friendly message to
     * the outside world or something of that kind, while needing to be
     * able to log more nasty and dirty details about its cause. This field
     * is supposed to keep that kind of data, optionally.
     * If the exception doesn't need a more detailed message, just
     * don't set it here, be free to just set the brief and public message.
     */
    protected final String details;

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
    public MappedException(String briefPublicMessage, String details){
        super(briefPublicMessage + " | " + details);
        this.briefPublicMessage = briefPublicMessage;
        this.details = details;
    }

    /**
     * Constructor method for exceptions that are just fine with only a
     * brief and public message, with no need of a more detailed
     * explanation. The 'message' attribute inherited from the
     * RuntimeException will be set only containing the brief public
     * message.
     * @param briefPublicMessage the brief and public info
     */
    public MappedException(String briefPublicMessage){
        super(briefPublicMessage);
        this.briefPublicMessage = briefPublicMessage;
        this.details = null;
    }

    /**
     * Getter method for the brief public message.
     * @return the brief and public message available.
     */
    public String getBriefPublicMessage(){
        return this.briefPublicMessage;
    }

    /**
     * Getter method for the more detailed explanation of the exception.
     * @return Optional of the detailed message.
     */
    public Optional<String> getDetails() {
        return Optional.ofNullable(details);
    }
}
