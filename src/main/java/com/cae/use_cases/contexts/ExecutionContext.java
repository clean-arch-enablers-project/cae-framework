package com.cae.use_cases.contexts;


import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.contexts.actors.Actor;
import com.cae.use_cases.contexts.exceptions.CorrelationIdValueFormatException;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.*;

/**
 * Represents the execution context of a {@code UseCase} or operation, encapsulating metadata such as:
 * <ul>
 *     <li>{@code correlationId} – a unique identifier for the execution instance,</li>
 *     <li>{@code actor} – the user or system entity responsible for the execution (optional),</li>
 *     <li>{@code stepInsights} – tracking insights for each inner execution step,</li>
 *     <li>{@code sharedContext} – a globally accessible map of stateless shared objects.</li>
 * </ul>
 * <p>
 * The shared context is provided by the {@link SharedContextProvider} singleton
 * and should be populated during application bootstrap.
 * </p>
 */
@Getter
public class ExecutionContext extends GenericExecutionManager {

    private final UUID correlationId;
    private final Actor actor;
    private final List<StepInsight> stepInsights = new ArrayList<>();
    private final Map<String, Object> sharedContext;

    /**
     * Creates a new execution context with the given correlation ID and no actor.
     *
     * @param correlationId the unique identifier for the execution context
     */
    public ExecutionContext(UUID correlationId) {
        this.correlationId = correlationId;
        this.actor = null;
        this.sharedContext = SharedContextProvider.SINGLETON.getSharedContext();
    }

    /**
     * Creates a new execution context with the given correlation ID and actor.
     *
     * @param correlationId the unique identifier for the execution context
     * @param actor the actor (user/system) responsible for the execution
     */
    public ExecutionContext(UUID correlationId, Actor actor){
        this.correlationId = correlationId;
        this.actor = actor;
        this.sharedContext = SharedContextProvider.SINGLETON.getSharedContext();
    }

    /**
     * Creates a new {@code ExecutionContext} from a string representation of a UUID (the unique identifier for the execution context).
     *
     * @param stringValue the string value representing a UUID (the unique identifier for the execution context)
     * @return a new {@code ExecutionContext} with the parsed UUID
     * @throws CorrelationIdValueFormatException if the string is not a valid UUID
     */
    public static ExecutionContext of(String stringValue){
        try {
            var uuid = UUID.fromString(stringValue);
            return new ExecutionContext(uuid);
        } catch (Exception exception){
            throw new CorrelationIdValueFormatException(stringValue);
        }
    }

    /**
     * Creates a new {@code ExecutionContext} from a string UUID (the unique identifier for the execution context) and an actor.
     *
     * @param stringValue the string value representing a UUID (the unique identifier for the execution context)
     * @param actor the actor associated with the execution
     * @return a new {@code ExecutionContext} with the parsed UUID and given actor
     * @throws CorrelationIdValueFormatException if the string is not a valid UUID
     */
    public static ExecutionContext of(String stringValue, Actor actor){
        try {
            var uuid = UUID.fromString(stringValue);
            return new ExecutionContext(uuid, actor);
        } catch (Exception exception){
            throw new CorrelationIdValueFormatException(stringValue);
        }
    }

    /**
     * Creates a new {@code ExecutionContext} with a randomly generated correlation ID.
     *
     * @return a new {@code ExecutionContext} with a random correlation ID
     */
    public static ExecutionContext ofNew(){
        return new ExecutionContext(UUID.randomUUID());
    }

    /**
     * Creates a new {@code ExecutionContext} with a random correlation ID and an actor.
     *
     * @param actor the actor responsible for the execution
     * @return a new {@code ExecutionContext} with a random correlation ID and the given actor
     */
    public static ExecutionContext ofNew(Actor actor) {
        return new ExecutionContext(UUID.randomUUID(), actor);
    }

    /**
     * Returns the actor associated with this execution context, if available.
     *
     * @return an {@code Optional} containing the actor, or empty if none is set
     */
    public Optional<Actor> getActor(){
        return Optional.ofNullable(this.actor);
    }

    /**
     * Creates and adds a new {@link StepInsight} associated with the given step name.
     * This method is only allowed after the execution context has been started.
     *
     * @param stepName the name of the step being tracked
     * @return the created {@code StepInsight}
     * @throws InternalMappedException if the execution context has not been started
     */
    public StepInsight addStepInsightsOf(String stepName){
        if (this.hasStarted()){
            var newStepInsight = StepInsight.of(stepName);
            this.stepInsights.add(newStepInsight);
            return newStepInsight;
        }
        throw new InternalMappedException(
                "Couldn't add step insight",
                "The execution context hasn't started yet"
        );
    }

    /**
     * Retrieves a shared object from the shared context by its key and type.
     *
     * @param key the key used to identify the object
     * @param objectType the expected type of the object
     * @param <T> the type to cast the object to
     * @return an {@code Optional} containing the object if found and castable, otherwise empty
     */
    public <T> Optional<T> getSharedContextObject(String key, Class<T> objectType){
        return Optional.ofNullable(this.sharedContext.get(key))
                .map(objectType::cast);
    }

    /**
     * Returns the string representation of the {@code correlationId}.
     *
     * @return the string value of the correlation ID
     */
    @Override
    public String toString(){
        return this.correlationId.toString();
    }

    /**
     * Represents insights for a single execution step within an {@link ExecutionContext},
     * including tracking of start time, latency, and exception (if any).
     */
    @Getter
    @Setter
    @NoArgsConstructor(access = AccessLevel.PRIVATE)
    public static class StepInsight extends GenericExecutionManager{

        /**
         * Creates and initializes a new {@code StepInsight} for the given step name.
         *
         * @param subject the name of the step
         * @return the initialized {@code StepInsight}
         */
        public static StepInsight of(String subject){
            var newInsight = new StepInsight();
            newInsight.setSubjectAndStartTracking(subject, false);
            return newInsight;
        }

        /**
         * Returns a human-readable description of the step insight,
         * including latency and exception details if any.
         *
         * @return a formatted string representing the step insight
         */
        @Override
        public String toString(){
            return this.getSubject()
                    + "'s insights: ("
                    + this.calculateLatency().toString() + "ms) "
                    + (this.wasSuccessful()? "no exception has been thrown" : ("an exception has been thrown along the way: " + this.getException()));
        }

    }

}
