package com.cae.loggers.formats;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class PortLogStructuredFormat {

    private PortExecutionLogFormat portExecution;

    @Getter
    @Builder
    public static class PortExecutionLogFormat{
        private String adapterName;
        private String correlationId;
        private IO io;
    }

}
