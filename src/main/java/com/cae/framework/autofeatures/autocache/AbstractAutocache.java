package com.cae.framework.autofeatures.autocache;

import com.cae.framework.autofeatures.autocache.eviction.AutocacheEvictionStrategy;
import com.cae.framework.autofeatures.autocache.eviction.AutocacheEvictionStrategyFactory;
import com.cae.framework.autofeatures.autocache.eviction.AutocacheEvictionTypes;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.context.ExecutionContext;
import com.cae.framework.workflows.Workflows;

import java.util.Optional;
import java.util.concurrent.*;

import static com.cae.framework.autofeatures.AutofeatureThreadPoolProvider.AutofeatureThreadFactory;

public abstract class AbstractAutocache<V> implements Cache<V>{

    protected AbstractAutocache(
            String ownerName,
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
        this.ownerName = ownerName;
        this.ttl = ttl;
        this.ttlTimeUnit = ttlTimeUnit;
        this.ttlBasedCleanupFrequency = ttlBasedCleanupFrequency;
        this.ttlBasedCleanupFrequencyTimeUnit = ttlBasedCleanupFrequencyTimeUnit;
        var ttlCleanupThreadFactory = new AutofeatureThreadFactory("AutocacheTTLCleanUpThreadPool");
        this.ttlBasedCleanupExecutor = Executors.newSingleThreadScheduledExecutor(ttlCleanupThreadFactory);
        this.maxSize = maxSize;
        this.evictionStrategy = AutocacheEvictionStrategyFactory.createNewFor(evictionType, maxSize);
        this.evictionBasedCleanupFrequency = evictionBasedCleanupFrequency;
        this.evictionBasedCleanupFrequencyTimeUnit = evictionBasedCleanupFrequencyTimeUnit;
        var evictionCleanupThreadFactory = new AutofeatureThreadFactory("AutocacheEvictionCleanUpThreadPool");
        this.evictionBasedCleanupExecutor = Executors.newSingleThreadScheduledExecutor(evictionCleanupThreadFactory);
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
        Runtime.getRuntime().addShutdownHook(
                new Thread(() -> this.shutdown(executor),
                this.ownerName + scope + "AutocacheShutdownHook")
        );
    }

    private final String ownerName;
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
    public void put(String cacheKey, V cacheValue, ExecutionContext executionContext) {
        Runnable action = () -> this.items.putIfAbsent(cacheKey, AutocacheItem.of(cacheValue, this.ttl, this.ttlTimeUnit));
        Workflows.of(executionContext).runStepOf("Autocache::put", action);
    }

    @Override
    public Optional<V> get(String cacheKey, ExecutionContext executionContext) {
        var workflow = Workflows.of(executionContext);
        var value = workflow.runStepOf(
                "Autocache::get",
                () -> Optional.ofNullable(this.items.get(cacheKey)).map(AutocacheItem::getValue)
        );
        value.ifPresent(actualItem -> workflow.runStepOf(
                "Autocache::keyUsageUpdate",
                () -> this.evictionStrategy.registerKeyUsage(cacheKey))
        );
        return value;
    }

    protected void removeExpiredItems() {
        var workflow = Workflows.ofNew(this.ownerName+"::autocacheTTLBasedCleanup", false);
        try{
            this.items.forEach((key, value) -> {
                if (value.isExpired()){
                    synchronized (this.removalLock){
                        workflow.runStepOf("TTLBasedItemRemoval", () -> this.items.remove(key));
                        workflow.runStepOf("TTLBasedKeyPurge", () -> this.evictionStrategy.forgetKey(key));
                    }
                }
            });
        } finally {
            workflow.commitToPostExecAutofeatures();
        }
    }

    protected void removeExceededItems() {
        var workflow = Workflows.ofNew(this.ownerName+"::autocacheEvictionBasedCleanup", false);
        try{
            while (this.items.size() > this.maxSize){
                var nextKeyToRemove = workflow.runStepOf("KeyToEvictRetrieval", this.evictionStrategy::getKeyToEvict);
                synchronized (this.removalLock){
                    var item = workflow.runStepOf("ItemToEvictRetrieval", this.items::get, nextKeyToRemove);
                    var removed = workflow.runStepOf("EvictionBasedItemRemoval", () -> this.items.remove(nextKeyToRemove, item));
                    if (item != null && removed)
                        workflow.runStepOf("EvictionBasedKeyPurge", () -> this.evictionStrategy.forgetKey(nextKeyToRemove));
                }
            }
        } finally {
            workflow.commitToPostExecAutofeatures();
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
