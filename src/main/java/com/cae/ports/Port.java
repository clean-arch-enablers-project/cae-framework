package com.cae.ports;

import com.cae.loggers.IOLoggingHandler;
import com.cae.loggers.LoggerProvider;
import com.cae.loggers.formats.IO;
import com.cae.loggers.formats.PortLogStructuredFormat;
import com.cae.loggers.native_io_extraction_mode.json.SimpleJsonBuilder;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import lombok.Getter;

import java.util.concurrent.CompletableFuture;

/**
 * Ports are meant to be the bridge between your core components
 * and the outside world. Ports come in a couple flavours: functions,
 * consumers, suppliers and runnables. Those subtypes are available
 * as specifics.
 */
@Getter
public abstract class Port {

    /**
     * Name of the implementation. If you need to find something
     * by ID within some use case implementation, accessing a database
     * or an API for that purpose, you would call a port from there instead
     * of calling the finding-by-id method implementation. The name of
     * the class would be set here, in this attribute, based on the class name
     * itself.
     */
    protected final String name;

    protected Port() {
        this.name = this.getClass().getSimpleName();
    }

    protected void handleIOLogs(Object input, Object output, UseCaseExecutionCorrelation correlation){
        var completableFuture = CompletableFuture.runAsync(() -> {
            var loggerProvider = LoggerProvider.SINGLETON;
            if (Boolean.TRUE.equals(loggerProvider.getPortsLoggingIO())){
                var logRow = Boolean.TRUE.equals(loggerProvider.getStructuredFormat())? this.generateStructureLogFor(input, output, correlation) : this.generateLogRowFor(input, output, correlation);
                loggerProvider.getProvidedInstance()
                        .orElseThrow(() -> new InternalMappedException("No logger instance provided.", "Please provide an instance via the LoggerProvider"))
                        .logInfo(logRow);
            }
        });
        if (Boolean.FALSE.equals(LoggerProvider.SINGLETON.getAsync()))
            completableFuture.join();
    }

    private String generateStructureLogFor(Object input, Object output, UseCaseExecutionCorrelation correlation) {
        var io = IO.builder()
                .input(input)
                .output(output)
                .build();
        var portExecution = PortLogStructuredFormat.PortExecutionLogFormat.builder()
                .adapterName(this.name)
                .correlationId(correlation.toString())
                .io(io)
                .build();
        var logMaterial = PortLogStructuredFormat.builder()
                .portExecution(portExecution)
                .build();
        return SimpleJsonBuilder.buildFor(logMaterial);
    }

    private String generateLogRowFor(Object input, Object output, UseCaseExecutionCorrelation correlation) {
        return this.name +
                " I/O data for correlation of \"" +
                correlation.getId().toString() +
                "\" — " +
                (input == null ? "" : IOLoggingHandler.generateTextForLoggingInput(input, "PORT")) +
                (output == null ? "" : IOLoggingHandler.generateTextForLoggingOutput(output, "PORT"));
    }

}
