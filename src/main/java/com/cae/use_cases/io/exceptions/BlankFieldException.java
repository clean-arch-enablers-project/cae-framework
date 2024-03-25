package com.cae.use_cases.io.exceptions;

import com.cae.mapped_exceptions.specifics.InputMappedException;

public class BlankFieldException extends InputMappedException {
    public BlankFieldException(String name) {
        super("Field '" + name + "' can't be blank.");
    }
}
