package com.cae.autofeatures.autometrics;

import com.cae.use_cases.contexts.ExecutionContext;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@RequiredArgsConstructor
@Getter
public class Metric {

    public static List<Metric> createNewOnesBasedOn(ExecutionContext executionContext) {
        var newMetrics = new ArrayList<Metric>();
        var mainMetric = new Metric(
            executionContext.getCorrelationId(),
            executionContext.getSubject(),
            executionContext.calculateLatency(),
            executionContext.getStartTime(),
            executionContext.getEndTime(),
            executionContext.wasSuccessful(),
            executionContext.getException(),
            true
        );
        newMetrics.add(mainMetric);
        executionContext.getStepInsights()
                .stream()
                .map(step -> new Metric(
                    executionContext.getCorrelationId(),
                    step.getSubject(),
                    step.calculateLatency(),
                    step.getStartTime(),
                    step.getEndTime(),
                    step.wasSuccessful(),
                    step.getException(),
                    false
                ))
                .forEach(newMetrics::add);
        return newMetrics;
    }

    private final UUID correlationId;
    private final String subjectName;
    private final Long latency;
    private final Instant startingTime;
    private final Instant endingTime;
    private final Boolean success;
    private final Exception exception;
    private final Boolean inbound;
}
