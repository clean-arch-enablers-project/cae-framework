package com.cae.autofeatures.autocache;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.time.Instant;
import java.util.concurrent.TimeUnit;

@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
@Getter
public class AutocacheItem<V>{

    public static <V> AutocacheItem<V> of(V value, Integer maxAge, TimeUnit maxAgeTimeUnit){
        return new AutocacheItem<>(
            value,
            Instant.now().plusMillis(maxAgeTimeUnit.toMillis(maxAge))
        );
    }

    private final V value;
    private final Instant expirationInstant;

    public boolean isExpired(){
        return Instant.now().isAfter(this.expirationInstant);
    }

}
