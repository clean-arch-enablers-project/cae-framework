package com.cae.autofeatures.autocache;

import com.cae.autofeatures.PostExecutionAutofeaturesRunner;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.use_cases.contexts.ExecutionContext;

import java.util.Optional;
import java.util.concurrent.*;

import static com.cae.autofeatures.AutofeatureThreadPoolProvider.AutofeatureThreadFactory;

public abstract class AbstractAutocache<V> implements Cache<V>{

    protected AbstractAutocache(
            String name,
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
        this.name = name;
        this.ttl = ttl;
        this.ttlTimeUnit = ttlTimeUnit;
        this.ttlBasedCleanupFrequency = ttlBasedCleanupFrequency;
        this.ttlBasedCleanupFrequencyTimeUnit = ttlBasedCleanupFrequencyTimeUnit;
        this.ttlBasedCleanupExecutor = Executors.newSingleThreadScheduledExecutor(new AutofeatureThreadFactory("AutocacheTTLCleanUpThreadPool"));
        this.maxSize = maxSize;
        this.evictionStrategy = AutocacheEvictionStrategyFactory.createNewFor(evictionType, maxSize);
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
        Runtime.getRuntime().addShutdownHook(
                new Thread(() -> this.shutdown(executor),
                this.name + scope + "AutocacheShutdownHook")
        );
    }

    private final String name;
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
        var execContext = ExecutionContext.ofNew();
        execContext.setSubjectAndStartTracking(this.name + "::autocacheTTLBasedCleanup");
        this.items.forEach((key, value) -> {
            if (value.isExpired()){
                synchronized (this.removalLock){
                    this.removeItemBasedOnTTL(key, execContext);
                    this.forgetKeyAtTheEvictionStrategyLevel(key, execContext);
                }
            }
        });
        execContext.complete();
        PostExecutionAutofeaturesRunner.runOn(execContext);
    }

    private void removeItemBasedOnTTL(String key, ExecutionContext execContext) {
        var removalStep = execContext.addStepInsightsOf("TTLBasedCacheKeyRemoval");
        removalStep.setInput(key);
        var removed = this.items.remove(key);
        removalStep.setOutput(removed);
        removalStep.complete();
    }

    protected void removeExceededItems() {
        var execContext = ExecutionContext.ofNew();
        execContext.setSubjectAndStartTracking(this.name + "::autocacheEvictionBasedCleanup");
        while (this.items.size() > this.maxSize){
            var nextKeyToRemove = this.findNextKeyToRemoveBasedOnEviction(execContext);
            synchronized (this.removalLock){
                var item = this.items.get(nextKeyToRemove);
                var removed = this.removeItemBasedOnEviction(nextKeyToRemove, item, execContext);
                if (item != null && removed) {
                    this.forgetKeyAtTheEvictionStrategyLevel(nextKeyToRemove, execContext);
                }
            }
        }
        execContext.complete();
        PostExecutionAutofeaturesRunner.runOn(execContext);
    }

    private String findNextKeyToRemoveBasedOnEviction(ExecutionContext execContext) {
        var findingKeyToEvictStep = execContext.addStepInsightsOf("EvictionBasedCacheKeyToRemoveRetrieval");
        var nextKeyToRemove = this.evictionStrategy.getKeyToEvict();
        findingKeyToEvictStep.setOutput(nextKeyToRemove);
        findingKeyToEvictStep.complete();
        return nextKeyToRemove;
    }

    private boolean removeItemBasedOnEviction(String nextKeyToRemove, AutocacheItem<V> item, ExecutionContext execContext) {
        var keyRemoval = execContext.addStepInsightsOf("EvictionBasedCacheKeyRemoval");
        keyRemoval.setInput(nextKeyToRemove);
        var removed = this.items.remove(nextKeyToRemove, item);
        keyRemoval.setOutput(removed);
        keyRemoval.complete();
        return removed;
    }

    private void forgetKeyAtTheEvictionStrategyLevel(String key, ExecutionContext execContext) {
        var keyForgivenessStep = execContext.addStepInsightsOf("CacheKeyForgiveness");
        keyForgivenessStep.setInput(key);
        this.evictionStrategy.forgetKey(key);
        keyForgivenessStep.complete();
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
