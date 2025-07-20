package com.cae.autofeatures.autocache;

import com.cae.mapped_exceptions.specifics.InternalMappedException;

import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

public class AutocacheEvictionStrategyFactory {

    private static final Map<AutocacheEvictionTypes, Function<Integer, AutocacheEvictionStrategy>> BLUEPRINT = Map.of(
            AutocacheEvictionTypes.LFU, LFUStrategy::new,
            AutocacheEvictionTypes.LRU, LRUStrategy::new
    );

    public static AutocacheEvictionStrategy createNewFor(AutocacheEvictionTypes type, Integer maxSize){
        return Optional.ofNullable(BLUEPRINT.get(type))
                .map(constructor -> constructor.apply(maxSize))
                .orElseThrow(() -> new InternalMappedException(
                        "Couldn't create new object for AutocacheEvictionStrategy based on the provided type",
                        "The " + type.name() + " is not set in the blueprint at AutocacheEvictionStrategyFactory"
                ));
    }

}
