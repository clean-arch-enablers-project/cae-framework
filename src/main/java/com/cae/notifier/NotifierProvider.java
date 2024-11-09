package com.cae.notifier;

import com.cae.mapped_exceptions.specifics.*;
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
    private Boolean considerUnexpectedExceptions = false;
    private final Set<Class<? extends Exception>> customExceptionsToConsider = ConcurrentHashMap.newKeySet();
    private Boolean considerLatency = false;
    private Integer latencyThreshold;

    public NotifierProvider setProvidedInstance(Notifier notifier){
        this.providedInstance = notifier;
        return this;
    }

    public Optional<Notifier> getProvidedInstance(){
        return Optional.ofNullable(this.providedInstance);
    }

    public NotifierProvider considerInputMappedExceptions(){
        this.customExceptionsToConsider.add(InputMappedException.class);
        return this;
    }

    public NotifierProvider considerNotFoundMappedExceptions(){
        this.customExceptionsToConsider.add(NotFoundMappedException.class);
        return this;
    }

    public NotifierProvider considerNotAuthenticatedMappedExceptions(){
        this.customExceptionsToConsider.add(NotAuthenticatedMappedException.class);
        return this;
    }

    public NotifierProvider considerNotAuthorizedMappedExceptions(){
        this.customExceptionsToConsider.add(NotAuthorizedMappedException.class);
        return this;
    }

    public NotifierProvider considerInternalMappedExceptions(){
        this.customExceptionsToConsider.add(InternalMappedException.class);
        return this;
    }

    public NotifierProvider considerUnexpectedExceptions(){
        this.considerUnexpectedExceptions = true;
        return this;
    }

    public <T extends Exception> NotifierProvider consider(Class<T> exceptionType){
        this.customExceptionsToConsider.add(exceptionType);
        return this;
    }

    public NotifierProvider considerLatency(Integer threshold){
        this.considerLatency = true;
        this.latencyThreshold = threshold;
        return this;
    }



}
