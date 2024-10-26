package com.cae.use_cases.contexts;


import com.cae.use_cases.contexts.actors.Actor;
import com.cae.use_cases.contexts.exceptions.CorrelationIdValueFormatException;
import lombok.Getter;

import java.util.Optional;
import java.util.UUID;

/**
 * Meant to be the unique identifier of a specific instance of
 * use case execution. In other words, each execution of any use case
 * is supposed to have a unique identifier, in order to make it easier
 * to track the status of any execution in logs or so.
 */
@Getter
public class ExecutionContext {

    /**
     * Its unique ID. UUIDs are pretty much impossible to conceive a
     * repeated value ever.
     */
    private final UUID correlationId;

    private final Actor actor;

    /**
     * Default constructor
     * @param correlationId receives an instance of UUID to set it directly as the ID
     */
    public ExecutionContext(UUID correlationId) {
        this.correlationId = correlationId;
        this.actor = null;
    }

    public ExecutionContext(UUID correlationId, Actor actor){
        this.correlationId = correlationId;
        this.actor = actor;
    }

    /**
     * Constructor for String values as parameter for the ID
     * @param stringValue string value that must be in UUID format
     * @return the Correlation instance
     */
    public static ExecutionContext of(String stringValue){
        try {
            var uuid = UUID.fromString(stringValue);
            return new ExecutionContext(uuid);
        } catch (Exception exception){
            throw new CorrelationIdValueFormatException(stringValue);
        }
    }

    public static ExecutionContext of(String stringValue, Actor actor){
        try {
            var uuid = UUID.fromString(stringValue);
            return new ExecutionContext(uuid, actor);
        } catch (Exception exception){
            throw new CorrelationIdValueFormatException(stringValue);
        }
    }

    /**
     * Constructor for auto-generating an ID value when it is fit
     * @return the Correlation instance
     */
    public static ExecutionContext ofNew(){
        return new ExecutionContext(UUID.randomUUID());
    }

    public static ExecutionContext ofNew(Actor actor) {
        return new ExecutionContext(UUID.randomUUID(), actor);
    }

    public Optional<Actor> getActor(){
        return Optional.ofNullable(this.actor);
    }

    /**
     * @return the UUID string value
     */
    @Override
    public String toString(){
        return this.correlationId.toString();
    }

}
