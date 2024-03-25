package com.cae.use_cases.io.exceptions;

import com.cae.mapped_exceptions.specifics.InternalMappedException;

public class NotEmptyAnnotationOnWrongTypeException extends InternalMappedException {
    public NotEmptyAnnotationOnWrongTypeException(String name) {
        super("The not-empty annotation should only be applied upon String fields.", "There is an issue about it at field '" + name + "'");
    }
}
