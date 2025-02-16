package com.cae.autolog.formats;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class PortInsightStructuredFormat {

    private PortExecutionLogFormat portExecution;

    @Getter
    @Builder
    public static class PortExecutionLogFormat{
        private String adapterName;
        private IO io;
    }

}
