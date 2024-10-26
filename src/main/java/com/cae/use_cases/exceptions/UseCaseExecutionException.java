package com.cae.use_cases.exceptions;

import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.UseCase;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Optional;

/**
 * If anything gets out of hand during a use case execution, this exception
 * is expected to be thrown.
 */
public class UseCaseExecutionException extends InternalMappedException {
    public UseCaseExecutionException(UseCase useCase, Exception unexpectedException) {
        super(
                "Something went unexpectedly wrong while executing use case of '" + useCase.getUseCaseMetadata().getName() + "'",
                "More details on the unexpected problem: "
                        + unexpectedException + "\nStack Trace: " + UseCaseExecutionException.getStackTraceAsString(unexpectedException)
                        + Optional.ofNullable(unexpectedException.getCause()).map(throwable -> "\nInformed cause: " + unexpectedException.getCause()).orElse("")
        );
    }

    public static String getStackTraceAsString(Exception exception){
        var stringWriter = new StringWriter();
        exception.printStackTrace(new PrintWriter(stringWriter));
        return stringWriter.toString();
    }

}
