package com.cae.autofeatures.autolog.formats;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
@Builder
public class UseCaseLogStructuredFormat {

    private UseCaseExecutionLogFormat useCaseExecution;

    @Getter
    @Setter
    @Builder
    public static class UseCaseExecutionLogFormat{
        private String useCase;
        private String correlationId;
        private IO io;
        private Boolean successful;
        private Long latency;
        private String exception;
        private List<String> portInsights;
    }

}
