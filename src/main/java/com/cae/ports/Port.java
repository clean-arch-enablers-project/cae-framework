package com.cae.ports;

import com.cae.loggers.IOLoggingHandler;
import com.cae.loggers.LoggerProvider;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;

/**
 * Ports are meant to be the bridge between your core components
 * and the outside world. Ports come in a couple flavours: functions,
 * consumers, suppliers and runnables. Those subtypes are available
 * as specifics.
 */
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

    public String getName(){
        return this.name;
    }

    protected void handleIOLogs(Object input, Object output, UseCaseExecutionCorrelation correlation){
        if (Boolean.TRUE.equals(LoggerProvider.SINGLETON.getPortsLoggingIO())){
            var logRow = this.generateLogRowFor(input, output, correlation);
            LoggerProvider.SINGLETON
                    .getProvidedInstance()
                    .orElseThrow(() -> new InternalMappedException("No logger instance provided.", "Please provide an instance via the LoggerProvider"))
                    .logInfo(logRow);
        }
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
