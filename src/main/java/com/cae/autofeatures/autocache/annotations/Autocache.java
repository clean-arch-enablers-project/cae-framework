package com.cae.autofeatures.autocache.annotations;

import com.cae.autofeatures.autocache.AutocacheEvictionTypes;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.concurrent.TimeUnit;

@Retention(RetentionPolicy.RUNTIME)
@Target(value = ElementType.TYPE)
public @interface Autocache {

    int ttl();
    TimeUnit ttlTimeUnit();
    int size() default 50;
    int cleanupFrequency() default 5;
    TimeUnit cleanupFrequencyTimeout() default TimeUnit.MINUTES;
    AutocacheEvictionTypes evictionType();
}
