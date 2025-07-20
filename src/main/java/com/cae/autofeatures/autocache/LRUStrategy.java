package com.cae.autofeatures.autocache;

public class LRUStrategy implements AutocacheEvictionStrategy {


    @Override
    public void registerKeyUsage(String key) {

    }

    @Override
    public String getKeyToEvict() {
        return "";
    }

    @Override
    public void forgetKey(String key) {

    }
}
