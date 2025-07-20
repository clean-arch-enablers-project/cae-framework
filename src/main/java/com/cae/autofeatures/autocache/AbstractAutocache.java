package com.cae.autofeatures.autocache;

import com.cae.mapped_exceptions.specifics.InternalMappedException;

import java.util.Optional;
import java.util.concurrent.*;

import static com.cae.autofeatures.AutofeatureThreadPoolProvider.AutofeatureThreadFactory;

public abstract class AbstractAutocache<V> implements Cache<V>{

    protected AbstractAutocache(
            Integer ttl,
            TimeUnit ttlTimeUnit,
            Integer maxSize,
            Integer cleanupFrequency,
            AutocacheEvictionTypes evictionType,
            TimeUnit cleanupFrequencyTimeUnit){
        if (maxSize < 2)
            throw new InternalMappedException(
                "Couldn't instantiate cache",
                "Max size was lesser than 2"
            );
        this.ttl = ttl;
        this.ttlTimeUnit = ttlTimeUnit;
        this.maxSize = maxSize;
        this.cleanupFrequency = cleanupFrequency;
        this.cleanupFrequencyTimeUnit = cleanupFrequencyTimeUnit;
        this.evictionStrategy = AutocacheEvictionStrategyFactory.createNewFor(evictionType);
        this.executor = Executors.newSingleThreadScheduledExecutor(new AutofeatureThreadFactory("AutocacheCleanUpThreadPool"));
        this.scheduleCleanUp();
    }

    private void scheduleCleanUp() {
        this.executor.scheduleAtFixedRate(
            this::removeExpiredItems,
            this.cleanupFrequency,
            this.cleanupFrequency,
            this.cleanupFrequencyTimeUnit
        );
    }

    private final Object putLock = new Object();
    private final Integer ttl;
    private final TimeUnit ttlTimeUnit;
    private final Integer maxSize;
    private final Integer cleanupFrequency;
    private final TimeUnit cleanupFrequencyTimeUnit;
    private final AutocacheEvictionStrategy evictionStrategy;
    private final ScheduledExecutorService executor;
    private final ConcurrentMap<String, AutocacheItem<V>> items = new ConcurrentHashMap<>();

    @Override
    public void put(String cacheKey, V cacheValue) {
        synchronized (this.putLock){
            if (this.items.containsKey(cacheKey))
                return;
            if (this.items.size() >= this.maxSize){
                var keyToRemove = this.evictionStrategy.getKeyToEvict();
                this.items.remove(keyToRemove);
                this.evictionStrategy.forgetKey(keyToRemove);
            }
            this.items.put(cacheKey, AutocacheItem.of(cacheValue, this.ttl, this.ttlTimeUnit));
        }
    }

    @Override
    public Optional<V> get(String cacheKey) {
        var value = Optional.ofNullable(this.items.get(cacheKey))
                .map(AutocacheItem::getValue);
        value.ifPresent(actualItem -> this.evictionStrategy.registerKeyUsage(cacheKey));
        return value;
    }

    private void removeExpiredItems() {
        this.items.forEach((key, value) -> {
            if (value.isExpired()){
                this.items.remove(key);
                this.evictionStrategy.forgetKey(key);
            }
        });
    }

}
