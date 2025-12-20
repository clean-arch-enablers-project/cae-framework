package com.cae.framework.workflows;

import com.cae.context.ExecutionContext;
import com.cae.framework.autofeatures.PostExecutionAutofeaturesRunner;
import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public class Workflows {

    public static Workflows ofNew(String subjectName, boolean isInbound){
        var execContext = ExecutionContext.ofNew();
        execContext.setSubjectAndStartTracking(subjectName, isInbound);
        return Workflows.of(execContext);
    }

    public static Workflows of(ExecutionContext executionContext){
        return new Workflows(executionContext);
    }

    private final ExecutionContext executionContext;
    private Exception exception;

    public  <I, O> O runStepOf(String stepName, Function<I, O> action, I actionInput){
        var step = this.executionContext.addStepInsightsOf(stepName);
        try {
            var actionOutput = action.apply(actionInput);
            step.complete();
            step.setOutput(actionOutput);
            return actionOutput;
        } catch (Exception exception){
            this.exception = exception;
            step.complete(exception);
            throw exception;
        } finally {
            step.setInput(actionInput);
        }
    }

    public  <I> void runStepOf(String stepName, Consumer<I> action, I actionInput){
        var step = this.executionContext.addStepInsightsOf(stepName);
        try {
            action.accept(actionInput);
            step.complete();
        } catch (Exception exception){
            this.exception = exception;
            step.complete(exception);
            throw exception;
        } finally {
            step.setInput(actionInput);
        }
    }

    public <O> O runStepOf(String stepName, Supplier<O> action){
        var step = this.executionContext.addStepInsightsOf(stepName);
        try {
            var actionOutput = action.get();
            step.complete();
            step.setOutput(actionOutput);
            return actionOutput;
        } catch (Exception exception){
            this.exception = exception;
            step.complete(exception);
            throw exception;
        }
    }

    public void runStepOf(String stepName, Runnable action){
        var step = this.executionContext.addStepInsightsOf(stepName);
        try {
            action.run();
            step.complete();
        } catch (Exception exception){
            this.exception = exception;
            step.complete(exception);
            throw exception;
        }
    }

    public void commitToPostExecAutofeatures(){
        if (this.exception == null)
            this.executionContext.complete();
        else
            this.executionContext.complete(this.exception);
        PostExecutionAutofeaturesRunner.runOn(this.executionContext);
    }

}
