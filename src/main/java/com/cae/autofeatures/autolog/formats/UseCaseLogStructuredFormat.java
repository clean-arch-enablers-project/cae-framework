package com.cae.autofeatures.autolog.formats;

import lombok.Builder;
import lombok.Getter;

import java.util.List;

@Getter
@Builder
public class UseCaseLogStructuredFormat {

    private UseCaseExecutionLogFormat useCaseExecution;

    @Getter
    @Builder
    public static class UseCaseExecutionLogFormat{
        private final String useCase;
        private final String correlationId;
        private final IO io;
        private final Boolean successful;
        private final Long latency;
        private final String exception;
        private final List<String> steps;
    }

}
