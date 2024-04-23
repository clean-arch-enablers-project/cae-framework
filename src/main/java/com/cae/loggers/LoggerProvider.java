package com.cae.loggers;

import com.cae.mapped_exceptions.specifics.InternalMappedException;

import java.util.Optional;

public class LoggerProvider {
    public static final LoggerProvider SINGLETON = new LoggerProvider();
    private LoggerProvider(){}

    private Logger providedInstance;
    private Boolean logIO = false;
    private IOLogMode ioLogMode = IOLogMode.TO_STRING;
    private Boolean portsLoggingIO = false;

    public Optional<Logger> getProvidedInstance(){
        return Optional.ofNullable(this.providedInstance);
    }

    public LoggerProvider setProvidedInstance(Logger providedInstance){
        this.providedInstance = providedInstance;
        return this;
    }

    public Boolean getLogIO(){
        return this.logIO;
    }

    public LoggerProvider setLogIO(IOLogMode ioLogMode){
        this.logIO = true;
        if (ioLogMode == IOLogMode.CAE_NATIVE){
            throw new InternalMappedException(
                    "Sorry, the \"IOLogMode.CAE_NATIVE\" option is not available on this the pre-release version, even though already being exposed via the API. See more details to know how to fix your LoggerProvider settings.",
                    "More details: instead of using the \"IOLogMode.CAE_NATIVE\" option, use the \"IOLogMode.TO_STRING\" one. This way the cae-framework will use the default or custom \"toString\" implementation of your I/O objects to extract their data. Don't worry though, soon enough the CAE_NATIVE mode will be available."
            );
        }
        this.ioLogMode = ioLogMode;
        return this;
    }

    public LoggerProvider setPortsLoggingIO(Boolean portsLoggingIO){
        this.portsLoggingIO = portsLoggingIO;
        return this;
    }

    public IOLogMode getMode(){
        return this.ioLogMode;
    }

    public Boolean getPortsLoggingIO(){
        return this.portsLoggingIO;
    }

    public enum IOLogMode {
        CAE_NATIVE,
        TO_STRING
    }

}
