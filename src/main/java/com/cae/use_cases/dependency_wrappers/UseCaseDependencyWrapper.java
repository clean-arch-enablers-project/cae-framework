package com.cae.use_cases.dependency_wrappers;

import com.cae.mapped_exceptions.specifics.InternalMappedException;

import java.util.Optional;

public abstract class UseCaseDependencyWrapper {

    protected <D> D getValueWithNullSafety(D dependency){
        return Optional.ofNullable(dependency).orElseThrow(() -> new NullReferenceIntoUseCaseDependencyException(this));
    }

    public static class NullReferenceIntoUseCaseDependencyException extends InternalMappedException {
        public NullReferenceIntoUseCaseDependencyException(UseCaseDependencyWrapper dependencyWrapper) {
            super("Use case must not receive one of its dependencies as null", dependencyWrapper.getClass().getSimpleName() + "had one of its attributes as null");
        }
    }
}
