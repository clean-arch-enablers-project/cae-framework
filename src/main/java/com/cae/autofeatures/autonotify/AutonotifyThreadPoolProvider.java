package com.cae.autofeatures.autonotify;

import com.cae.autofeatures.AutofeatureThreadPoolProvider;

public class AutonotifyThreadPoolProvider extends AutofeatureThreadPoolProvider {

    public static final AutonotifyThreadPoolProvider SINGLETON = new AutonotifyThreadPoolProvider();

    protected AutonotifyThreadPoolProvider() {
        super("AutonotifyThreadPool");
    }
}