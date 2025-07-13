package com.cae.autofeatures.autolog;

import lombok.Getter;

import java.util.Optional;

@Getter
public class AutologProvider {

    private AutologProvider(){}

    public static final AutologProvider SINGLETON = new AutologProvider();

    private Logger providedInstance;
    private Boolean async = true;
    private Boolean useCasesLoggingIO = false;
    private Boolean portsLoggingIO = false;
    private IOAutologMode ioAutologMode = IOAutologMode.CAE_NATIVE;
    private Boolean structuredFormat = false;
    private Boolean logStackTrace = false;
    private Integer linesOfStackTrace = 5;

    public void reset(){
        this.providedInstance = null;
        this.useCasesLoggingIO = false;
        this.async = true;
        this.portsLoggingIO = false;
        this.ioAutologMode = IOAutologMode.CAE_NATIVE;
        this.structuredFormat = false;
        this.logStackTrace = false;
        this.linesOfStackTrace = 5;
    }

    public Optional<Logger> getProvidedInstance(){
        return Optional.ofNullable(this.providedInstance);
    }

    public AutologProvider setProvidedInstance(Logger providedInstance){
        this.providedInstance = providedInstance;
        return this;
    }

    public AutologProvider setAsync(Boolean async){
        this.async = async;
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

    public AutologProvider setIOAutologMode(IOAutologMode ioAutologMode){
        this.ioAutologMode = ioAutologMode;
        return this;
    }

    public AutologProvider setStructuredFormat(boolean structuredFormat){
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
