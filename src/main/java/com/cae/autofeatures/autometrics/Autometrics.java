package com.cae.autofeatures.autometrics;

import com.cae.use_cases.contexts.ExecutionContext;

public class Autometrics {

    public static void runOn(ExecutionContext executionContext) {
        AutometricsThreadPoolProvider.SINGLETON.getExecutor()
                .submit(() -> Metric.createNewOnesBasedOn(executionContext).forEach(Autometrics::send));
    }

    public static void send(Metric metric){
        AutometricsProvider.SINGLETON.getSubscribers()
                .forEach(subscriber -> subscriber.receiveMetric(metric));
    }

}
