package com.cae.autofeatures.autometrics;

import com.cae.autofeatures.AutofeatureThreadPoolProvider;

public class AutometricsThreadPoolProvider extends AutofeatureThreadPoolProvider {

    public static final AutometricsThreadPoolProvider SINGLETON = new AutometricsThreadPoolProvider();

    protected AutometricsThreadPoolProvider() {
        super("AutometricsThreadPool");
    }
}