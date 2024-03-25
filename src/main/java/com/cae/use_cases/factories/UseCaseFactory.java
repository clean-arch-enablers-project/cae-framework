package com.cae.use_cases.factories;

import com.cae.use_cases.UseCase;

/**
 * Contract for establishing the pattern of instantiating use cases
 * @param <U> the use case type
 */
public interface UseCaseFactory <U extends UseCase>{

    /**
     * Method that is responsible for having all the logic necessary
     * for instantiating the use case. Feel free to even add other methods
     * to your UseCaseFactory implementations with fancier method
     * signatures.
     * @return the use case instance
     */
    U makeInstance();
}
