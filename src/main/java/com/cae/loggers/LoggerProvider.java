package com.cae.loggers;

import java.util.Optional;

public class LoggerProvider {

    private LoggerProvider(){}

    public static final LoggerProvider SINGLETON = new LoggerProvider();

    private Logger providedInstance;
    private Boolean useCasesLoggingIO = false;
    private Boolean portsLoggingIO = false;
    private IOLoggingMode ioLoggingMode = IOLoggingMode.TO_STRING;
    private Boolean async = false;

    public Optional<Logger> getProvidedInstance(){
        return Optional.ofNullable(this.providedInstance);
    }

    public Boolean getUseCasesLoggingIO(){
        return this.useCasesLoggingIO;
    }

    public Boolean getPortsLoggingIO(){
        return this.portsLoggingIO;
    }

    public Boolean getAsync(){
        return this.async;
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

    public LoggerProvider async(boolean async){
        this.async = async;
        return this;
    }

}
