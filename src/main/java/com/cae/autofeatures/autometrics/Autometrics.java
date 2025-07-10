package com.cae.autofeatures.autometrics;

import com.cae.use_cases.contexts.ExecutionContext;

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
    }

}
