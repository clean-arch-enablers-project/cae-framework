package com.cae.framework.autofeatures.autometrics;

import com.cae.framework.autofeatures.AutofeatureThreadPoolProvider;

public class AutometricsThreadPoolProvider extends AutofeatureThreadPoolProvider {

    public static final AutometricsThreadPoolProvider SINGLETON = new AutometricsThreadPoolProvider();

    protected AutometricsThreadPoolProvider() {
        super("AutometricsThreadPool");
    }
}