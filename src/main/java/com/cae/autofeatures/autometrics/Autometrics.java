package com.cae.autofeatures.autometrics;

import java.util.concurrent.CompletableFuture;

public class Autometrics {

    public static void collect(Metric metric){
        var executor = AutometricsThreadPoolProvider.SINGLETON.getExecutor();
        AutometricsProvider.SINGLETON
                .getSubscribers()
                .forEach(subscriber -> CompletableFuture.runAsync(() -> subscriber.receiveMetric(metric), executor));
    }

}
