package com.cae.autofeatures.autocache;

import java.util.concurrent.TimeUnit;

public class DefaultCaeAutocache<V> extends AbstractAutocache<V> {

    public DefaultCaeAutocache(
            Integer ttl,
            TimeUnit ttlTimeUnit,
            Integer ttlBasedCleanupFrequency,
            TimeUnit ttlBasedCleanupFrequencyTimeUnit,
            Integer maxSize,
            AutocacheEvictionTypes evictionType,
            Integer evictionBasedCleanupFrequency,
            TimeUnit evictionBasedCleanupFrequencyTimeUnit
            ) {
        super(
                ttl,
                ttlTimeUnit,
                ttlBasedCleanupFrequency,
                ttlBasedCleanupFrequencyTimeUnit,
                maxSize,
                evictionType,
                evictionBasedCleanupFrequency,
                evictionBasedCleanupFrequencyTimeUnit
        );
    }

}
