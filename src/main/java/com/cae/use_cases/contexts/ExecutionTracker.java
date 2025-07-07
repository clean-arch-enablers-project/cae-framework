package com.cae.use_cases.contexts;

import com.cae.mapped_exceptions.specifics.InternalMappedException;
import lombok.Getter;

import java.time.Duration;
import java.time.Instant;
import java.util.Optional;

@Getter
public abstract class ExecutionTracker {

    protected Instant startTime;
    protected Instant endTime;
    protected Long latency;

    protected void startTracking(){
        this.startTime = Instant.now();
    }

    protected void endTracking(){
        if (this.hasStarted())
            this.endTime = Instant.now();
        else
            throw new InternalMappedException(
                "Couldn't end tracking",
                "It hasn't started yet"
            );
    }

    protected boolean hasStarted(){
        return this.startTime != null;
    }

    protected boolean isComplete(){
        return this.endTime != null;
    }

    protected Long getLatency(){
        if (this.isComplete())
            return Optional.ofNullable(this.latency)
                    .orElseGet(() -> {
                        this.latency = Duration.between(this.startTime, this.endTime).toMillis();
                        return this.latency;
                    });
        throw new InternalMappedException(
                "Couldn't calculate latency",
                "Either the startTime or the endTime was null (or even both). Make sure to try it only after having the tracking complete"
        );
    }

}
