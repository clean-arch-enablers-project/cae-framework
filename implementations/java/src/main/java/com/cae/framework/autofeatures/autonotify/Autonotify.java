package com.cae.framework.autofeatures.autonotify;

import com.cae.context.ExecutionContext;
import com.cae.framework.initializers.Lazy;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Autonotify {

    public static void runOn(ExecutionContext executionContext) {
        boolean shouldRunAsynchronously = AutonotifyProvider.SINGLETON.getAsync();
        Runnable action = () -> Notification.createNewOnesBasedOn(executionContext).forEach(Autonotify::send);
        if (shouldRunAsynchronously)
            AutonotifyThreadPoolProvider.SINGLETON.getExecutor().submit(action);
        else
            action.run();
    }

    public static void send(Notification notification) {
        AutonotifyProvider.SINGLETON.getSubscribers()
                .forEach(subscriber -> subscriber.receiveNotification(notification));
        AutonotifyProvider.SINGLETON.getLazySubscribers()
                .stream()
                .map(Lazy::get)
                .forEach(lazySubscriber -> lazySubscriber.receiveNotification(notification));
    }
}
