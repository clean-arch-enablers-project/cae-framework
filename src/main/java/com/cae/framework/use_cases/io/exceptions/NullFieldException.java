package com.cae.framework.use_cases.io.exceptions;


import com.cae.mapped_exceptions.specifics.InputMappedException;

public class NullFieldException extends InputMappedException {
    public NullFieldException(String name) {
        super("Field '" + name + "' can't be null");
    }
}
