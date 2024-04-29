package com.cae.loggers;

import lombok.Getter;

import java.util.Optional;

@Getter
public class LoggerProvider {

    private LoggerProvider(){}

    public static final LoggerProvider SINGLETON = new LoggerProvider();

    private Logger providedInstance;
    private Boolean useCasesLoggingIO = false;
    private Boolean portsLoggingIO = false;
    private IOLoggingMode ioLoggingMode = IOLoggingMode.CAE_NATIVE;
    private Boolean async = true;
    private Boolean structuredFormat = false;

    public Optional<Logger> getProvidedInstance(){
        return Optional.ofNullable(this.providedInstance);
    }

    public IOLoggingMode getIOLoggingMode(){
        return this.ioLoggingMode;
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

    public LoggerProvider setIOLoggingMode(IOLoggingMode ioLoggingMode){
        this.ioLoggingMode = ioLoggingMode;
        return this;
    }

    public LoggerProvider structuredFormat(boolean structuredFormat){
        this.structuredFormat = structuredFormat;
        return this;
    }

    public LoggerProvider async(boolean async){
        this.async = async;
        return this;
    }

}
