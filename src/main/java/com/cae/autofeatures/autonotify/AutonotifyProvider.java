package com.cae.autofeatures.autonotify;

import com.cae.env_vars.exceptions.MissingEnvVarException;
import com.cae.mapped_exceptions.specifics.*;
import com.cae.trier.autoretry.NoRetriesLeftException;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
@Getter(AccessLevel.PROTECTED)
public class AutonotifyProvider {

    public static final AutonotifyProvider SINGLETON = new AutonotifyProvider();

    private final List<AutonotifySubscriber> subscribers = new ArrayList<>();
    private Boolean considerUnexpectedExceptions = false;
    private final Set<Class<? extends Exception>> exceptionsToConsider = ConcurrentHashMap.newKeySet();
    private Boolean considerLatency = false;
    private Integer latencyThreshold;

    public AutonotifyProvider setAllSubscribers(List<AutonotifySubscriber> autonotifySubscribers){
        this.subscribers.addAll(autonotifySubscribers);
        return this;
    }

    public AutonotifyProvider setSubscriber(AutonotifySubscriber autonotifySubscriber){
        this.subscribers.add(autonotifySubscriber);
        return this;
    }

    public AutonotifyProvider considerInputMappedExceptions(){
        this.exceptionsToConsider.add(InputMappedException.class);
        return this;
    }

    public AutonotifyProvider considerNotFoundMappedExceptions(){
        this.exceptionsToConsider.add(NotFoundMappedException.class);
        return this;
    }

    public AutonotifyProvider considerNotAuthenticatedMappedExceptions(){
        this.exceptionsToConsider.add(NotAuthenticatedMappedException.class);
        return this;
    }

    public AutonotifyProvider considerNotAuthorizedMappedExceptions(){
        this.exceptionsToConsider.add(NotAuthorizedMappedException.class);
        return this;
    }

    public AutonotifyProvider considerInternalMappedExceptions(){
        this.exceptionsToConsider.add(InternalMappedException.class);
        return this;
    }

    public AutonotifyProvider considerNoRetriesLeftExceptions(){
        this.exceptionsToConsider.add(NoRetriesLeftException.class);
        return this;
    }

    public AutonotifyProvider considerMissingEnvVarExceptions(){
        this.exceptionsToConsider.add(MissingEnvVarException.class);
        return this;
    }

    public AutonotifyProvider considerUnexpectedExceptions(){
        this.considerUnexpectedExceptions = true;
        return this;
    }

    public <T extends Exception> AutonotifyProvider considerSpecifically(Class<T> exceptionType){
        this.exceptionsToConsider.add(exceptionType);
        return this;
    }

    public AutonotifyProvider considerLatency(Integer threshold){
        this.considerLatency = true;
        this.latencyThreshold = threshold;
        return this;
    }



}
