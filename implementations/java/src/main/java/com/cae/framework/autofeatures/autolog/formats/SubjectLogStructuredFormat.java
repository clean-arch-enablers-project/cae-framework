package com.cae.framework.autofeatures.autolog.formats;

import lombok.Builder;
import lombok.Getter;

import java.util.List;

@Getter
@Builder
public class SubjectLogStructuredFormat {

    private SubjectExecutionLogFormat execution;

    @Getter
    @Builder
    public static class SubjectExecutionLogFormat {
        private final String subject;
        private final String correlationId;
        private final IO io;
        private final Boolean successful;
        private final Long latency;
        private final String exception;
        private final List<String> steps;
    }

}
