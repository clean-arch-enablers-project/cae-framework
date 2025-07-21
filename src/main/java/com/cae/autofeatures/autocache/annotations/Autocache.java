package com.cae.autofeatures.autocache.annotations;

import com.cae.autofeatures.autocache.eviction.AutocacheEvictionTypes;

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
    int ttlBasedCleanupFrequency() default 5;
    TimeUnit ttlBasedCleanupFrequencyTimeUnit() default TimeUnit.MINUTES;
    int size() default 50;
    AutocacheEvictionTypes evictionType();
    int evictionBasedCleanupFrequency() default 250;
    TimeUnit evictionBasedCleanupFrequencyTimeUnit() default TimeUnit.MILLISECONDS;
}
