package com.cae.loggers.native_io_extraction_mode.json.sensitive;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
public @interface Sensitive {

    int unmaskedAmount() default 0;
    boolean unmaskFromLeft() default true;
    int defaultMaskedAmount() default 0;
}
