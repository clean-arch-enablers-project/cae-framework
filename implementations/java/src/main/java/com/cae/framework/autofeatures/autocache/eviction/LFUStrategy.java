package com.cae.framework.autofeatures.autocache.eviction;

import com.cae.mapped_exceptions.specifics.InternalMappedException;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicInteger;

@RequiredArgsConstructor
public class LFUStrategy implements AutocacheEvictionStrategy {

    @Getter
    private final Integer maxSize;
    private final ConcurrentMap<String, Item> registry = new ConcurrentHashMap<>();

    @Override
    public void registerKeyUsage(String key) {
        this.registry.computeIfAbsent(key, Item::of).incrementUsage();
    }

    @Override
    public String getKeyToEvict() {
        var snapshot = new ArrayList<>(this.registry.values());
        return snapshot.stream()
                .sorted()
                .findFirst()
                .map(Item::getKey)
                .orElseThrow(() -> new InternalMappedException(
                    "Couldn't get key to evict",
                    "For some reason there were no keys available when it came the time to evict. Probably some error at the AbstractAutocache layer"
                ));
    }

    @Override
    public void forgetKey(String key) {
        this.registry.remove(key);
    }

    @RequiredArgsConstructor(access = AccessLevel.PRIVATE)
    protected static class Item implements Comparable<Item>{

        public static Item of(String key){
            return new Item(key, new AtomicInteger(0));
        }

        @Getter
        private final String key;
        private final AtomicInteger value;

        @Override
        public int compareTo(Item o) {
            return Integer.compare(this.value.get(), o.value.get());
        }

        public void incrementUsage(){
            this.value.getAndIncrement();
        }

    }

}
