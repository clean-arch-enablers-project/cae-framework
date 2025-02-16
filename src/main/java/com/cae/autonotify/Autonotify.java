package com.cae.autonotify;

import com.cae.mapped_exceptions.specifics.*;
import com.cae.ports.Port;
import com.cae.use_cases.UseCase;
import com.cae.use_cases.contexts.ExecutionContext;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.Optional;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class NotifierManager {

    public static void handleNotificationOn(UseCase useCase, Exception exception, Long latency, ExecutionContext context){
        AutonotifyProvider.SINGLETON.getProvidedInstance().ifPresent(notifier -> NotifierManager.handle(
                notifier,
                useCase.getUseCaseMetadata().getName(),
                exception,
                latency,
                context
        ));
    }

    public static void handleNotificationOn(Port port, Exception exception, Long latency, ExecutionContext context){
        AutonotifyProvider.SINGLETON.getProvidedInstance().ifPresent(notifier -> NotifierManager.handle(
                notifier,
                port.getName(),
                exception,
                latency,
                context
        ));
    }

    private static void handle(
            Notifier notifier,
            String subject,
            Exception exception,
            Long latency,
            ExecutionContext context){
        var notificationSettings = AutonotifyProvider.SINGLETON;
        var reasons = new ArrayList<String>();
        if (Boolean.TRUE.equals(notificationSettings.getConsiderLatency()) && notificationSettings.getLatencyThreshold() < latency)
            reasons.add("Latency threshold: " + notificationSettings.getLatencyThreshold() + "ms allowed vs. actually " + latency + "ms");
        Optional.ofNullable(exception).ifPresent(actualException -> {
            if (notificationSettings.getCustomExceptionsToConsider().contains(actualException.getClass()))
                reasons.add("An exception was thrown during its execution. It specifically matched one of the parameterized types set for notification.");
            else if (NotifierManager.checkWhetherGenericallyMatchesMappedExceptions(notificationSettings, actualException))
                reasons.add("An exception was thrown during its execution. It generically matched one of the MappedException types set for notification.");
            else if (Boolean.TRUE.equals(notificationSettings.getConsiderUnexpectedExceptions()))
                reasons.add("An unexpected exception was thrown during its execution.");
        });
        if (!reasons.isEmpty()) {
            var notification = Notifier.Notification.builder()
                    .subject(subject)
                    .executionContext(context)
                    .exception(exception)
                    .reasons(reasons)
                    .build();
            notifier.emitNotification(notification);
        }
    }

    private static boolean checkWhetherGenericallyMatchesMappedExceptions(
            AutonotifyProvider notificationSettings,
            Exception actualException) {
        var map = notificationSettings.getCustomExceptionsToConsider();
        return
                (map.contains(InternalMappedException.class) && actualException instanceof InternalMappedException)
                ||
                (map.contains(InputMappedException.class) && actualException instanceof InputMappedException)
                ||
                (map.contains(NotFoundMappedException.class) && actualException instanceof NotFoundMappedException)
                ||
                (map.contains(NotAuthenticatedMappedException.class) && actualException instanceof NotAuthenticatedMappedException)
                ||
                (map.contains(NotAuthorizedMappedException.class) && actualException instanceof NotAuthorizedMappedException);

    }

}
