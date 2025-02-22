package com.cae.autonotify;

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

    private final List<NotificationObserver> providedInstances = new ArrayList<>();
    private Boolean considerUnexpectedExceptions = false;
    private final Set<Class<? extends Exception>> customExceptionsToConsider = ConcurrentHashMap.newKeySet();
    private Boolean considerLatency = false;
    private Integer latencyThreshold;

    public AutonotifyProvider setAllProvidedInstances(List<NotificationObserver> notificationObservers){
        this.providedInstances.addAll(notificationObservers);
        return this;
    }

    public AutonotifyProvider setProvidedInstance(NotificationObserver notificationObserver){
        this.providedInstances.add(notificationObserver);
        return this;
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

    public AutonotifyProvider considerNoRetriesLeftExceptions(){
        this.customExceptionsToConsider.add(NoRetriesLeftException.class);
        return this;
    }

    public AutonotifyProvider considerMissingEnvVarExceptions(){
        this.customExceptionsToConsider.add(MissingEnvVarException.class);
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
