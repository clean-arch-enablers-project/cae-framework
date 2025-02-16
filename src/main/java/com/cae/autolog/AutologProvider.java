package com.cae.loggers;

import lombok.Getter;

import java.util.Optional;

@Getter
public class AutologProvider {

    private AutologProvider(){}

    public static final AutologProvider SINGLETON = new AutologProvider();

    private Logger providedInstance;
    private Boolean useCasesLoggingIO = false;
    private Boolean portsLoggingIO = false;
    private IOLoggingMode ioLoggingMode = IOLoggingMode.CAE_NATIVE;
    private final Boolean async = false;
    private Boolean structuredFormat = false;
    private Boolean logStackTrace = false;
    private Integer linesOfStackTrace = 5;

    public Optional<Logger> getProvidedInstance(){
        return Optional.ofNullable(this.providedInstance);
    }

    public IOLoggingMode getIOLoggingMode(){
        return this.ioLoggingMode;
    }

    public AutologProvider setProvidedInstance(Logger providedInstance){
        this.providedInstance = providedInstance;
        return this;
    }

    public AutologProvider setUseCasesLoggingIO(Boolean useCasesLoggingIO){
        this.useCasesLoggingIO = useCasesLoggingIO;
        return this;
    }

    public AutologProvider setPortsLoggingIO(Boolean portsLoggingIO){
        this.portsLoggingIO = portsLoggingIO;
        return this;
    }

    public AutologProvider setIOLoggingMode(IOLoggingMode ioLoggingMode){
        this.ioLoggingMode = ioLoggingMode;
        return this;
    }

    public AutologProvider structuredFormat(boolean structuredFormat){
        this.structuredFormat = structuredFormat;
        return this;
    }

    public AutologProvider setLoggingStackTrace(Boolean loggingStackTrace){
        this.logStackTrace = loggingStackTrace;
        return this;
    }

    public AutologProvider setNumberOfLinesFromStackTrace(Integer numberOfLines){
        this.linesOfStackTrace = numberOfLines;
        return this;
    }


}
