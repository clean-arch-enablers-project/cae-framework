package com.cae.framework.initializers.exceptions;

import com.cae.mapped_exceptions.specifics.InternalMappedException;

public class LazyInitializationMappedException extends InternalMappedException {
    public LazyInitializationMappedException(Exception exception) {
        super(
                "Couldn't lazily initialize an instance",
                "This issue happened exactly when attempted to lazily initialize one of your lazy instances: " + exception.getClass().getSimpleName(),
                exception
        );
    }
}
