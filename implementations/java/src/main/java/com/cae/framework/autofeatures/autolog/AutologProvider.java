package com.cae.framework.autofeatures.autolog;

import lombok.Getter;

import java.util.Optional;

@Getter
public class AutologProvider {

    private AutologProvider(){}

    public static final AutologProvider SINGLETON = new AutologProvider();

    private Logger providedInstance;
    private Boolean async = true;
    private Boolean subjectsLoggingIO = false;
    private Boolean innerStepsLoggingIO = false;
    private IOAutologMode ioAutologMode = IOAutologMode.CAE_NATIVE;
    private Boolean structuredFormat = false;
    private Boolean logStackTrace = false;
    private Integer linesOfStackTrace = 5;

    public void reset(){
        this.providedInstance = null;
        this.subjectsLoggingIO = false;
        this.async = true;
        this.innerStepsLoggingIO = false;
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

    public AutologProvider setSubjectsLoggingIO(Boolean subjectsLoggingIO){
        this.subjectsLoggingIO = subjectsLoggingIO;
        return this;
    }

    public AutologProvider setInnerStepsLoggingIO(Boolean innerStepsLoggingIO){
        this.innerStepsLoggingIO = innerStepsLoggingIO;
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
