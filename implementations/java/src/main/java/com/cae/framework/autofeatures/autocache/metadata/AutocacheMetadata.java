package com.cae.framework.autofeatures.autocache.metadata;

import com.cae.framework.autofeatures.autocache.eviction.AutocacheEvictionTypes;
import com.cae.framework.autofeatures.autocache.annotations.Autocache;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;

import java.util.Optional;
import java.util.concurrent.TimeUnit;

@Getter
@Builder(access = AccessLevel.PRIVATE)
public class AutocacheMetadata {

    public static AutocacheMetadata of(Autocache annotationInstance){
        return Optional.ofNullable(annotationInstance)
                .map(annotation -> AutocacheMetadata.builder()
                        .ttl(annotation.ttl())
                        .ttlTimeUnit(annotation.ttlTimeUnit())
                        .ttlBasedCleanupFrequency(annotation.ttlBasedCleanupFrequency())
                        .ttlBasedCleanupFrequencyTimeUnit(annotation.ttlBasedCleanupFrequencyTimeUnit())
                        .size(annotation.size())
                        .evictionType(annotation.evictionType())
                        .evictionBasedCleanupFrequency(annotation.evictionBasedCleanupFrequency())
                        .evictionBasedCleanupFrequencyTimeUnit(annotation.evictionBasedCleanupFrequencyTimeUnit())
                        .build())
                .orElse(null);
    }

    private final Integer ttl;
    private final TimeUnit ttlTimeUnit;
    private final Integer ttlBasedCleanupFrequency;
    private final TimeUnit ttlBasedCleanupFrequencyTimeUnit;
    private final Integer size;
    private final AutocacheEvictionTypes evictionType;
    private final Integer evictionBasedCleanupFrequency;
    private final TimeUnit evictionBasedCleanupFrequencyTimeUnit;

}
