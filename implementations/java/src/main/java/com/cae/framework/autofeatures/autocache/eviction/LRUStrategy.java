package com.cae.framework.autofeatures.autocache.eviction;

import com.cae.mapped_exceptions.specifics.InternalMappedException;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class LRUStrategy implements AutocacheEvictionStrategy {

    private static final Object DUMMY = new Object();

    public LRUStrategy(Integer maxCacheSize){
        var resizingFactor = 0.75f;
        var initialSize = (int) Math.ceil(maxCacheSize / resizingFactor);
        this.registry = new LinkedHashMap<>(
                initialSize,
                resizingFactor,
                true
        );
    }

    private final Map<String, Object> registry;
    private final ReentrantReadWriteLock genericLock = new ReentrantReadWriteLock();
    private final Lock readingLock = this.genericLock.readLock();
    private final Lock writingLock = this.genericLock.writeLock();

    @Override
    public void registerKeyUsage(String key) {
        this.writingLock.lock();
        try{
            this.registry.put(key, DUMMY);
        } finally {
            this.writingLock.unlock();
        }
    }

    @Override
    public String getKeyToEvict() {
        this.readingLock.lock();
        try {
            return this.registry.keySet()
                    .stream()
                    .findFirst()
                    .orElseThrow(() -> new InternalMappedException(
                        "Couldn't get key to evict",
                        "The registry for the LRU strategy was empty when trying to get key to evict. That's probably an error at the AbstractAutocache layer"
                    ));
        } finally {
            this.readingLock.unlock();
        }
    }

    @Override
    public void forgetKey(String key) {
        this.writingLock.lock();
        try {
            this.registry.remove(key);
        } finally {
            this.writingLock.unlock();
        }
    }
}
