package com.cae.framework.use_cases.boundaries;

import com.cae.framework.use_cases.metadata.OperationTypes;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(value = ElementType.TYPE)
public @interface Internal {

    String[] scopes() default {};
    String[] affectedOwners() default {};
    OperationTypes operationType() default OperationTypes.NOT_SPECIFIED;
}
