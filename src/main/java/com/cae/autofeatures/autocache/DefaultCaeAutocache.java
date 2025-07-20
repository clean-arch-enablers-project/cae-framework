package com.cae.autofeatures.autocache;

import java.util.concurrent.TimeUnit;

public class DefaultCaeAutocache<V> extends AbstractAutocache<V> {

    public DefaultCaeAutocache(
            String name,
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
                name,
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
