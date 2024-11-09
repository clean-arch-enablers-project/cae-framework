package com.cae.notifier;

import com.cae.ports.Port;
import com.cae.use_cases.UseCase;
import com.cae.use_cases.contexts.ExecutionContext;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class NotifierManager {

    public static void handleNotificationOn(UseCase useCase, Exception exception, Long latency, ExecutionContext context){
        NotifierProvider.SINGLETON.getProvidedInstance().ifPresent(notifier -> {

        });
    }

    public static void handleNotificationOn(Port port, Exception exception, Long latency, ExecutionContext context){
        NotifierProvider.SINGLETON.getProvidedInstance().ifPresent(notifier -> {

        });
    }

}
