package com.cae.use_cases.contexts;

import com.cae.mapped_exceptions.specifics.InternalMappedException;
import lombok.Getter;
import lombok.Setter;

@Getter
public abstract class GenericExecutionManager extends ExecutionTracker{

    protected String subject;
    protected boolean inbound;
    protected Exception exception;
    @Setter
    protected Object input;
    @Setter
    protected Object output;

    public void setSubjectAndStartTracking(String subject, boolean inbound){
        if (this.hasStarted())
            throw new InternalMappedException(
                    "Couldn't set subject and start tracking",
                    "The execution context you tried to start was already used"
            );
        this.subject = subject;
        this.inbound = inbound;
        this.startTracking();
    }

    public void complete(){
        this.endTracking();
    }

    public void complete(Exception exception){
        this.endTracking();
        this.exception = exception;
    }

    public Long calculateLatency(){
        return this.getLatency();
    }

    public boolean wasSuccessful(){
        if (this.isComplete())
            return this.exception == null;
        throw new InternalMappedException(
                "Couldn't check whether execution was successful",
                "It wasn't complete"
        );
    }

}
