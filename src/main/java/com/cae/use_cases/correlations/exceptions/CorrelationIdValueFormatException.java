package com.cae.use_cases.correlations.exceptions;

public class CorrelationIdValueFormatException extends RuntimeException {
    public CorrelationIdValueFormatException(String stringValue) {
        super("The string value '"+stringValue+"' cant be used as a correlation ID. Correlation IDs must be unique. For that reason it is decided they must be in UUID format.");
    }
}
