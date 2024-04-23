package com.cae.loggers;

import java.util.Optional;

public class LoggerProvider {

    private LoggerProvider(){}

    public static final LoggerProvider SINGLETON = new LoggerProvider();

    private Logger providedInstance;
    private Boolean useCasesLoggingIO = false;
    private Boolean portsLoggingIO = false;
    private static final IOLoggingMode IO_LOGGING_MODE = IOLoggingMode.TO_STRING;

    public Optional<Logger> getProvidedInstance(){
        return Optional.ofNullable(this.providedInstance);
    }

    public Boolean getUseCasesLoggingIO(){
        return this.useCasesLoggingIO;
    }

    public Boolean getPortsLoggingIO(){
        return this.portsLoggingIO;
    }

    public IOLoggingMode getIOLoggingMode(){
        return IO_LOGGING_MODE;
    }

    public LoggerProvider setProvidedInstance(Logger providedInstance){
        this.providedInstance = providedInstance;
        return this;
    }

    public LoggerProvider setUseCasesLoggingIO(Boolean useCasesLoggingIO){
        this.useCasesLoggingIO = useCasesLoggingIO;
        return this;
    }

    public LoggerProvider setPortsLoggingIO(Boolean portsLoggingIO){
        this.portsLoggingIO = portsLoggingIO;
        return this;
    }

    public enum IOLoggingMode {
        CAE_NATIVE,
        TO_STRING
    }

}
