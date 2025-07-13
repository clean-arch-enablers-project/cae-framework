package com.cae.autofeatures.autolog;

import com.cae.autofeatures.AutofeatureThreadPoolProvider;

public class AutologThreadPoolProvider extends AutofeatureThreadPoolProvider {

    public static final AutologThreadPoolProvider SINGLETON = new AutologThreadPoolProvider();

    protected AutologThreadPoolProvider() {
        super("AutologThreadPool");
    }
}