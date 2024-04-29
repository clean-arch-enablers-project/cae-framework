package com.cae.ports;

import com.cae.loggers.IOLoggingHandler;
import com.cae.loggers.LoggerProvider;
import com.cae.loggers.formats.IO;
import com.cae.loggers.formats.PortLogStructuredFormat;
import com.cae.loggers.native_io_extraction_mode.json.SimpleJsonBuilder;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;

import java.util.concurrent.CompletableFuture;

@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public class PortLoggingManagement {

    private final String portName;

    public static PortLoggingManagement of(String portName){
        return new PortLoggingManagement(portName);
    }

    public void handlePortExecutionLog(Object input, Object output, UseCaseExecutionCorrelation correlation){
        this.runSynchronouslyIfNecessary(CompletableFuture.runAsync(() -> {
            var loggerProvider = LoggerProvider.SINGLETON;
            if (Boolean.TRUE.equals(loggerProvider.getPortsLoggingIO())){
                var logRow = Boolean.TRUE.equals(loggerProvider.getStructuredFormat())? this.generateStructureLogFor(input, output, correlation) : this.generateLogRowFor(input, output, correlation);
                loggerProvider.getProvidedInstance()
                        .orElseThrow(() -> new InternalMappedException("No logger instance provided.", "Please provide an instance via the LoggerProvider"))
                        .logInfo(logRow);
            }
        }));
    }

    private void runSynchronouslyIfNecessary(CompletableFuture<Void> future){
        if (Boolean.FALSE.equals(LoggerProvider.SINGLETON.getAsync()))
            future.join();
    }

    private String generateStructureLogFor(Object input, Object output, UseCaseExecutionCorrelation correlation) {
        var io = IO.builder()
                .input(input)
                .output(output)
                .build();
        var portExecution = PortLogStructuredFormat.PortExecutionLogFormat.builder()
                .adapterName(this.portName)
                .correlationId(correlation.toString())
                .io(io)
                .build();
        var logMaterial = PortLogStructuredFormat.builder()
                .portExecution(portExecution)
                .build();
        return SimpleJsonBuilder.buildFor(logMaterial);
    }

    private String generateLogRowFor(Object input, Object output, UseCaseExecutionCorrelation correlation) {
        return this.portName +
                " I/O data for correlation of \"" +
                correlation.getId().toString() +
                "\" — " +
                this.generateInputPartOfLog(input) +
                this.generateOutputPartOfLog(output);
    }

    private String generateInputPartOfLog(Object input) {
        return (input == null ? "" : IOLoggingHandler.generateTextForLoggingInput(input, "PORT"));
    }

    private String generateOutputPartOfLog(Object output) {
        return (output == null ? "" : IOLoggingHandler.generateTextForLoggingOutput(output, "PORT"));
    }

}
