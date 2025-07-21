package com.cae.autofeatures.autocache;

import com.cae.autofeatures.autocache.metadata.AutocacheMetadata;

public class DefaultCaeAutocache<V> extends AbstractAutocache<V> {

    public DefaultCaeAutocache(String name, AutocacheMetadata autocacheMetadata) {
        super(
            name,
            autocacheMetadata.getTtl(),
            autocacheMetadata.getTtlTimeUnit(),
            autocacheMetadata.getTtlBasedCleanupFrequency(),
            autocacheMetadata.getTtlBasedCleanupFrequencyTimeUnit(),
            autocacheMetadata.getSize(),
            autocacheMetadata.getEvictionType(),
            autocacheMetadata.getEvictionBasedCleanupFrequency(),
            autocacheMetadata.getEvictionBasedCleanupFrequencyTimeUnit()
        );
    }

}
