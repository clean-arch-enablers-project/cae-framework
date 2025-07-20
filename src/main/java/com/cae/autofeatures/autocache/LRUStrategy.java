package com.cae.autofeatures.autocache;

import java.util.LinkedHashMap;

public class LRUStrategy implements AutocacheEvictionStrategy {

    private LinkedHashMap

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
