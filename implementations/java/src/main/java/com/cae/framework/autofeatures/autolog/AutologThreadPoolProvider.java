package com.cae.framework.autofeatures.autolog;

import com.cae.framework.autofeatures.AutofeatureThreadPoolProvider;

public class AutologThreadPoolProvider extends AutofeatureThreadPoolProvider {

    public static final AutologThreadPoolProvider SINGLETON = new AutologThreadPoolProvider();

    protected AutologThreadPoolProvider() {
        super("AutologThreadPool");
    }
}