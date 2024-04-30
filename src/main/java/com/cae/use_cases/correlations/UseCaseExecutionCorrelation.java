package com.cae.use_cases.correlations;


import com.cae.use_cases.correlations.actors.Actor;
import com.cae.use_cases.correlations.exceptions.CorrelationIdValueFormatException;
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
public class UseCaseExecutionCorrelation {

    /**
     * Its unique ID. UUIDs are pretty much impossible to conceive a
     * repeated value ever.
     */
    private final UUID id;

    private final Actor actor;

    /**
     * Default constructor
     * @param id receives an instance of UUID to set it directly as the ID
     */
    public UseCaseExecutionCorrelation(UUID id) {
        this.id = id;
        this.actor = null;
    }

    public UseCaseExecutionCorrelation(UUID id, Actor actor){
        this.id = id;
        this.actor = actor;
    }

    /**
     * Constructor for String values as parameter for the ID
     * @param stringValue string value that must be in UUID format
     * @return the Correlation instance
     */
    public static UseCaseExecutionCorrelation of(String stringValue){
        try {
            var uuid = UUID.fromString(stringValue);
            return new UseCaseExecutionCorrelation(uuid);
        } catch (Exception exception){
            throw new CorrelationIdValueFormatException(stringValue);
        }
    }

    public static UseCaseExecutionCorrelation of(String stringValue, Actor actor){
        try {
            var uuid = UUID.fromString(stringValue);
            return new UseCaseExecutionCorrelation(uuid, actor);
        } catch (Exception exception){
            throw new CorrelationIdValueFormatException(stringValue);
        }
    }

    /**
     * Constructor for auto-generating an ID value when it is fit
     * @return the Correlation instance
     */
    public static UseCaseExecutionCorrelation ofNew(){
        return new UseCaseExecutionCorrelation(UUID.randomUUID());
    }

    public static UseCaseExecutionCorrelation ofNew(Actor actor) {
        return new UseCaseExecutionCorrelation(UUID.randomUUID(), actor);
    }

    public Optional<Actor> getActor(){
        return Optional.ofNullable(this.actor);
    }

    /**
     * @return the UUID string value
     */
    @Override
    public String toString(){
        return this.id.toString();
    }

}
