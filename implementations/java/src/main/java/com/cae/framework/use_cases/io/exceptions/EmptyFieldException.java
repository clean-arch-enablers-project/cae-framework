package com.cae.framework.use_cases.io.exceptions;

import com.cae.mapped_exceptions.specifics.InputMappedException;

public class EmptyFieldException extends InputMappedException {
    public EmptyFieldException(String name) {
        super("Field '" + name + "' can't be empty");
    }
}
