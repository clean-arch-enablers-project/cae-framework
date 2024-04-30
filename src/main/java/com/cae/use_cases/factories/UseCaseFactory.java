package com.cae.use_cases.factories;

import com.cae.use_cases.UseCase;
import com.cae.use_cases.dependency_wrappers.UseCaseDependencyWrapper;

import java.util.Optional;

/**
 * Contract for establishing the pattern of instantiating use cases
 * @param <U> the use case type
 */
public abstract class UseCaseFactory <U extends UseCase, D extends UseCaseDependencyWrapper>{

    private U useCaseSingleton;

    /**
     * Method that is responsible for having all the logic necessary
     * for instantiating the use case. Feel free to even add other methods
     * to your UseCaseFactory implementations with fancier method
     * signatures.
     * @return the use case instance
     */
    public U getOrCreateDefaultSingletonInstance(D dependencyWrapper){
        return Optional.ofNullable(this.useCaseSingleton)
                .orElseGet(() -> {
                    this.useCaseSingleton = this.initializeSingletonUsing(dependencyWrapper);
                    return this.useCaseSingleton;
                });
    }

    protected abstract U initializeSingletonUsing(D dependencyWrapper);
}
