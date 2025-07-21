package com.cae.autofeatures.autocache;

import com.cae.use_cases.contexts.ExecutionContext;

import java.util.Optional;

public interface Cache<T> {

    void put(String cacheKey, T cacheValue, ExecutionContext executionContext);
    Optional<T> get(String cacheKey, ExecutionContext executionContext);

}
