package com.cae.autofeatures.autonotify;

import com.cae.use_cases.contexts.ExecutionContext;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Autonotify {

    public static void runOn(ExecutionContext executionContext) {
        AutonotifyThreadPoolProvider.SINGLETON.getExecutor()
                .submit(() -> Notification.createNewOnesBasedOn(executionContext).forEach(Autonotify::send));
    }

    public static void send(Notification notification) {
        AutonotifyProvider.SINGLETON.getSubscribers()
                .forEach(subscriber -> subscriber.receiveNotification(notification));
    }
}
