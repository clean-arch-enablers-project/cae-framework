package com.cae.autonotify;

import com.cae.mapped_exceptions.specifics.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
@Getter(AccessLevel.PROTECTED)
public class AutonotifyProvider {

    public static final AutonotifyProvider SINGLETON = new AutonotifyProvider();

    private Notifier providedInstance;
    private Boolean considerUnexpectedExceptions = false;
    private final Set<Class<? extends Exception>> customExceptionsToConsider = ConcurrentHashMap.newKeySet();
    private Boolean considerLatency = false;
    private Integer latencyThreshold;

    public AutonotifyProvider setProvidedInstance(Notifier notifier){
        this.providedInstance = notifier;
        return this;
    }

    public Optional<Notifier> getProvidedInstance(){
        return Optional.ofNullable(this.providedInstance);
    }

    public AutonotifyProvider considerInputMappedExceptions(){
        this.customExceptionsToConsider.add(InputMappedException.class);
        return this;
    }

    public AutonotifyProvider considerNotFoundMappedExceptions(){
        this.customExceptionsToConsider.add(NotFoundMappedException.class);
        return this;
    }

    public AutonotifyProvider considerNotAuthenticatedMappedExceptions(){
        this.customExceptionsToConsider.add(NotAuthenticatedMappedException.class);
        return this;
    }

    public AutonotifyProvider considerNotAuthorizedMappedExceptions(){
        this.customExceptionsToConsider.add(NotAuthorizedMappedException.class);
        return this;
    }

    public AutonotifyProvider considerInternalMappedExceptions(){
        this.customExceptionsToConsider.add(InternalMappedException.class);
        return this;
    }

    public AutonotifyProvider considerUnexpectedExceptions(){
        this.considerUnexpectedExceptions = true;
        return this;
    }

    public <T extends Exception> AutonotifyProvider considerSpecifically(Class<T> exceptionType){
        this.customExceptionsToConsider.add(exceptionType);
        return this;
    }

    public AutonotifyProvider considerLatency(Integer threshold){
        this.considerLatency = true;
        this.latencyThreshold = threshold;
        return this;
    }



}
