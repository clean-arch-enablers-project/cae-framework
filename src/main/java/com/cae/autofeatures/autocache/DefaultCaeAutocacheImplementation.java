package com.cae.autofeatures.autocache;

import java.util.concurrent.TimeUnit;

public class DefaultCaeAutocacheImplementation<V> extends AbstractAutocache<V> {

    public DefaultCaeAutocacheImplementation(
            Integer ttl,
            TimeUnit ttlTimeUnit,
            Integer maxSize,
            Integer cleanupFrequency,
            TimeUnit cleanupFrquencyTimeUnit) {
        super(ttl, ttlTimeUnit, maxSize, cleanupFrequency, cleanupFrquencyTimeUnit);
    }

}
