package com.cae.use_cases.contexts;


import com.cae.use_cases.contexts.actors.Actor;
import com.cae.use_cases.contexts.exceptions.CorrelationIdValueFormatException;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Getter
public class ExecutionContext extends GenericExecutionManager {

    private final UUID correlationId;
    private final Actor actor;
    private final List<StepInsight> stepInsights = new ArrayList<>();

    public ExecutionContext(UUID correlationId) {
        this.correlationId = correlationId;
        this.actor = null;
    }

    public ExecutionContext(UUID correlationId, Actor actor){
        this.correlationId = correlationId;
        this.actor = actor;
    }

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

    public static ExecutionContext ofNew(){
        return new ExecutionContext(UUID.randomUUID());
    }

    public static ExecutionContext ofNew(Actor actor) {
        return new ExecutionContext(UUID.randomUUID(), actor);
    }

    public Optional<Actor> getActor(){
        return Optional.ofNullable(this.actor);
    }

    public StepInsight addStepInsightsOf(String stepName){
        var newStepInsight = StepInsight.of(stepName);
        this.stepInsights.add(newStepInsight);
        return newStepInsight;
    }

    @Override
    public String toString(){
        return this.correlationId.toString();
    }

    @Getter
    @Setter
    public static class StepInsight extends GenericExecutionManager{

        public static StepInsight of(String subject){
            var newInsight = new StepInsight();
            newInsight.setSubjectAndStartTracking(subject);
            return newInsight;
        }

        @Override
        public String toString(){
            return this.getSubject()
                    + "'s insights: ("
                    + this.calculateLatency().toString() + "ms) "
                    + (this.wasSuccessful()? "no exception has been thrown" : ("an exception has been thrown along the way: " + this.getException()));
        }

    }

}
