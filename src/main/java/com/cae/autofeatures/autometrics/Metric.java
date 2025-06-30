package com.cae.autofeatures.autometrics;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter
public class Metric {

    public static Metric of(String subjectName, Long latency, Exception exception, Boolean inbound){
        return new Metric(
                subjectName,
                latency,
                exception == null,
                exception,
                inbound
        );
    }

    private final String subjectName;
    private final Long latency;
    private final Boolean success;
    private final Exception exception;
    private final Boolean inbound;

}
