package com.cae.framework.autofeatures.autometrics;

import com.cae.context.ExecutionContext;
import com.cae.framework.initializers.Lazy;

public class Autometrics {

    public static void runOn(ExecutionContext executionContext) {
        boolean shouldRunAsynchronously = AutometricsProvider.SINGLETON.getAsync();
        Runnable action = () -> Metric.createNewOnesBasedOn(executionContext).forEach(Autometrics::send);
        if (shouldRunAsynchronously)
            AutometricsThreadPoolProvider.SINGLETON.getExecutor().submit(action);
        else
            action.run();
    }

    public static void send(Metric metric){
        AutometricsProvider.SINGLETON.getSubscribers()
                .forEach(subscriber -> subscriber.receiveMetric(metric));
        AutometricsProvider.SINGLETON.getLazySubscribers()
                .stream()
                .map(Lazy::get)
                .forEach(lazySubscriber -> lazySubscriber.receiveMetric(metric));
    }

}
