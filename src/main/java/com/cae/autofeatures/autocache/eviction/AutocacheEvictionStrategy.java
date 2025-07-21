package com.cae.autofeatures.autocache.eviction;

public interface AutocacheEvictionStrategy {

    void registerKeyUsage(String key);
    String getKeyToEvict();
    void forgetKey(String key);
}
