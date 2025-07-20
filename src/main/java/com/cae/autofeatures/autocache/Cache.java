package com.cae.autofeatures.autocache;

import java.util.Optional;

public interface Cache<T> {

    void put(String cacheKey, T cacheValue);
    Optional<T> get(String cacheKey);

}
