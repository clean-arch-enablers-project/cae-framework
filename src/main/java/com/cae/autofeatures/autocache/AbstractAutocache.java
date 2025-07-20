package com.cae.autofeatures.autocache;

import com.cae.mapped_exceptions.specifics.InternalMappedException;

import java.util.Optional;
import java.util.concurrent.*;

import static com.cae.autofeatures.AutofeatureThreadPoolProvider.AutofeatureThreadFactory;

public abstract class AbstractAutocache<V> implements Cache<V>{

    protected AbstractAutocache(
            Integer ttl,
            TimeUnit ttlTimeUnit,
            Integer ttlBasedCleanupFrequency,
            TimeUnit ttlBasedCleanupFrequencyTimeUnit,
            Integer maxSize,
            AutocacheEvictionTypes evictionType,
            Integer evictionBasedCleanupFrequency,
            TimeUnit evictionBasedCleanupFrequencyTimeUnit){
        if (maxSize < 2)
            throw new InternalMappedException(
                "Couldn't instantiate cache",
                "Max size was lesser than 2"
            );
        this.ttl = ttl;
        this.ttlTimeUnit = ttlTimeUnit;
        this.ttlBasedCleanupFrequency = ttlBasedCleanupFrequency;
        this.ttlBasedCleanupFrequencyTimeUnit = ttlBasedCleanupFrequencyTimeUnit;
        this.ttlBasedCleanupExecutor = Executors.newSingleThreadScheduledExecutor(new AutofeatureThreadFactory("AutocacheTTLCleanUpThreadPool"));
        this.maxSize = maxSize;
        this.evictionStrategy = AutocacheEvictionStrategyFactory.createNewFor(evictionType);
        this.evictionBasedCleanupFrequency = evictionBasedCleanupFrequency;
        this.evictionBasedCleanupFrequencyTimeUnit = evictionBasedCleanupFrequencyTimeUnit;
        this.evictionBasedCleanupExecutor = Executors.newSingleThreadScheduledExecutor(new AutofeatureThreadFactory("AutocacheEvictionCleanUpThreadPool"));
        this.scheduleTtlBasedCleanup();
        this.scheduleEvictionBasedCleanup();
    }

    private void scheduleTtlBasedCleanup() {
        this.ttlBasedCleanupExecutor.scheduleAtFixedRate(
            this::removeExpiredItems,
            this.ttlBasedCleanupFrequency,
            this.ttlBasedCleanupFrequency,
            this.ttlBasedCleanupFrequencyTimeUnit
        );
        this.addShutdownHookFor(this.ttlBasedCleanupExecutor, "TTLBased");
    }

    private void scheduleEvictionBasedCleanup() {
        this.evictionBasedCleanupExecutor.scheduleWithFixedDelay(
            this::removeExceededItems,
            this.evictionBasedCleanupFrequency,
            this.evictionBasedCleanupFrequency,
            this.evictionBasedCleanupFrequencyTimeUnit
        );
        this.addShutdownHookFor(this.evictionBasedCleanupExecutor, "EvictionBased");
    }

    private void addShutdownHookFor(ScheduledExecutorService executor, String scope) {
        Runtime.getRuntime().addShutdownHook(new Thread(() -> this.shutdown(executor), scope+"AutocacheShutdownHook"));
    }

    private final Integer ttl;
    private final TimeUnit ttlTimeUnit;
    private final Integer maxSize;
    private final Integer ttlBasedCleanupFrequency;
    private final TimeUnit ttlBasedCleanupFrequencyTimeUnit;
    private final AutocacheEvictionStrategy evictionStrategy;
    private final ScheduledExecutorService ttlBasedCleanupExecutor;
    private final Integer evictionBasedCleanupFrequency;
    private final TimeUnit evictionBasedCleanupFrequencyTimeUnit;
    private final ScheduledExecutorService evictionBasedCleanupExecutor;
    private final ConcurrentMap<String, AutocacheItem<V>> items = new ConcurrentHashMap<>();
    private final Object removalLock = new Object();

    @Override
    public void put(String cacheKey, V cacheValue) {
        this.items.putIfAbsent(cacheKey, AutocacheItem.of(cacheValue, this.ttl, this.ttlTimeUnit));
    }

    @Override
    public Optional<V> get(String cacheKey) {
        var value = Optional.ofNullable(this.items.get(cacheKey))
                .map(AutocacheItem::getValue);
        value.ifPresent(actualItem -> this.evictionStrategy.registerKeyUsage(cacheKey));
        return value;
    }

    protected void removeExpiredItems() {
        this.items.forEach((key, value) -> {
            if (value.isExpired()){
                synchronized (this.removalLock){
                    this.items.remove(key);
                    this.evictionStrategy.forgetKey(key);
                }
            }
        });
    }

    protected void removeExceededItems() {
        while (this.items.size() > this.maxSize){
            var nextKeyToRemove = this.evictionStrategy.getKeyToEvict();
            var item = this.items.get(nextKeyToRemove);
            synchronized (this.removalLock){
                if (item != null && this.items.remove(nextKeyToRemove, item)) {
                    this.evictionStrategy.forgetKey(nextKeyToRemove);
                }
            }
        }
    }

    public void shutdown(){
        this.shutdown(this.ttlBasedCleanupExecutor);
        this.shutdown(this.evictionBasedCleanupExecutor);
    }

    private void shutdown(ScheduledExecutorService executor) {
        executor.shutdown();
        try{
            if (!executor.awaitTermination(3, TimeUnit.SECONDS))
                executor.shutdownNow();
        } catch (InterruptedException interruptedException){
            executor.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }


}
