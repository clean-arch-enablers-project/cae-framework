package com.cae.notifier;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
@Getter
public class NotifierProvider {

    public static final NotifierProvider SINGLETON = new NotifierProvider();

    private Notifier providedInstance;
    private Boolean considerInputMappedExceptions = false;
    private Boolean considerNotFoundMappedExceptions = false;
    private Boolean considerNotAuthenticatedMappedExceptions = false;
    private Boolean considerNotAuthorizedMappedExceptions = false;
    private Boolean considerInternalMappedExceptions = false;
    private Boolean considerUnexpectedExceptions;
    private final Set<Exception> customExceptionsToConsider = ConcurrentHashMap.newKeySet();
    private Boolean considerLatency = false;
    private Integer latencyThreshold = 500;

    public NotifierProvider setProvidedInstance(Notifier notifier){
        this.providedInstance = notifier;
        return this;
    }

    public Optional<Notifier> getProvidedInstance(){
        return Optional.ofNullable(this.providedInstance);
    }

    public NotifierProvider considerInputMappedExceptions(){
        this.considerInputMappedExceptions = true;
        return this;
    }

    public NotifierProvider considerNotFoundMappedExceptions(){
        this.considerNotFoundMappedExceptions = true;
        return this;
    }

    public NotifierProvider considerNotAuthenticatedMappedExceptions(){
        this.considerNotAuthenticatedMappedExceptions = true;
        return this;
    }

    public NotifierProvider considerNotAuthorizedMappedExceptions(){
        this.considerNotAuthorizedMappedExceptions = true;
        return this;
    }

    public NotifierProvider considerInternalMappedExceptions(){
        this.considerInternalMappedExceptions = true;
        return this;
    }

    public NotifierProvider considerUnexpectedExceptions(){
        this.considerUnexpectedExceptions = true;
        return this;
    }

    public NotifierProvider consider(Exception exception){
        this.customExceptionsToConsider.add(exception);
        return this;
    }

    public NotifierProvider considerLatency(Integer threshold){
        this.considerLatency = true;
        this.latencyThreshold = threshold;
        return this;
    }



}
