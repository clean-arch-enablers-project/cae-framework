package com.cae.use_cases;

import com.cae.mapped_exceptions.specifics.InternalMappedException;

/**
 * If anything gets out of hand during a use case execution, this exception
 * is expected to be thrown.
 */
public class UseCaseExecutionException extends InternalMappedException {
    public UseCaseExecutionException(UseCase useCase, Exception unexpectedException) {
        super(
                "Something went unexpectedly wrong while executing use case of '" + useCase.getUseCaseMetadata().getName() + "'",
                "More details on the unexpected problem: " + unexpectedException
        );
    }

}
