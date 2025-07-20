package com.cae.autofeatures.autocache;

public interface AutocacheEvictionStrategy {

    void registerKeyUsage(String key);
    String getKeyToEvict();
    void forgetKey(String key);
}
