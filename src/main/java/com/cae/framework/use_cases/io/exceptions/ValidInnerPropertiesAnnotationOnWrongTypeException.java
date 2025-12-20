package com.cae.framework.use_cases.io.exceptions;


import com.cae.mapped_exceptions.specifics.InternalMappedException;

public class ValidInnerPropertiesAnnotationOnWrongTypeException extends InternalMappedException {
    public ValidInnerPropertiesAnnotationOnWrongTypeException(String name) {
        super("The valid-inner-properties annotation should only be applied upon UseCaseInput and Collections of UseCaseInput typed fields.", "There is an issue about it" +
                " at field '" + name + "'");
    }
}
