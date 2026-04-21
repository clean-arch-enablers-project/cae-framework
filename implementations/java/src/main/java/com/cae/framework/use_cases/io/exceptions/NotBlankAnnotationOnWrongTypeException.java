package com.cae.framework.use_cases.io.exceptions;

import com.cae.mapped_exceptions.specifics.InternalMappedException;

public class NotBlankAnnotationOnWrongTypeException extends InternalMappedException {
    public NotBlankAnnotationOnWrongTypeException(String name) {
        super("The not-blank annotation should only be applied upon String or Collections of String fields.", "There is an issue about it at field '" + name + "'");
    }
}
