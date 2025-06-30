package com.cae.autofeatures.autoauth.exceptions;

import com.cae.mapped_exceptions.specifics.InputMappedException;
import com.cae.use_cases.UseCase;

public class NotAllowedMappedException extends InputMappedException {

    public NotAllowedMappedException(UseCase useCase) {
        super(
                "Actor not allowed to execute App:" + useCase.getClass().getSuperclass().getSimpleName()
        );
    }
}
