package com.cae.framework.autofeatures.autonotify;

import com.cae.framework.autofeatures.AutofeatureThreadPoolProvider;

public class AutonotifyThreadPoolProvider extends AutofeatureThreadPoolProvider {

    public static final AutonotifyThreadPoolProvider SINGLETON = new AutonotifyThreadPoolProvider();

    protected AutonotifyThreadPoolProvider() {
        super("AutonotifyThreadPool");
    }
}