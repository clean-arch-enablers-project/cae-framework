package com.cae.autonotify;

import com.cae.mapped_exceptions.specifics.*;
import com.cae.ports.Port;
import com.cae.use_cases.UseCase;
import com.cae.use_cases.contexts.ExecutionContext;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Autonotify {

    public static void handleNotificationOn(
            UseCase useCase,
            Exception exception,
            Long latency,
            ExecutionContext context){
        Autonotify.generateNotificationBy(
                useCase.getUseCaseMetadata().getName(),
                exception,
                latency,
                context
        ).ifPresent(Autonotify::notify);
    }

    public static void handleNotificationOn(
            Port port,
            Exception exception,
            Long latency,
            ExecutionContext context){
        Autonotify.generateNotificationBy(
                port.getName(),
                exception,
                latency,
                context
        ).ifPresent(Autonotify::notify);
    }

    public static void manuallyNotify(Notification notification){
        Autonotify.notify(notification);
    }

    private static void notify(Notification notification){
        var executor = AutonotifyThreadPoolProvider.SINGLETON.getExecutor();
        AutonotifyProvider.SINGLETON
                .getProvidedInstances()
                .forEach(notificationObserver -> CompletableFuture.runAsync(() -> notificationObserver.getNotified(notification), executor));
    }

    private static Optional<Notification> generateNotificationBy(
            String subject,
            Exception exception,
            Long latency,
            ExecutionContext context){
        var reasons = new ArrayList<String>();
        Autonotify.checkLatencyCriteria(latency, reasons);
        Optional.ofNullable(exception)
                .ifPresent(actualException -> Autonotify.checkExceptionsCriteria(actualException, reasons));
        if (reasons.isEmpty())
            return Optional.empty();
        var notification = Notification.builder()
                .subject(subject)
                .executionContext(context)
                .exception(exception)
                .reasons(reasons)
                .build();
        return Optional.ofNullable(notification);
    }

    private static void checkLatencyCriteria(
            Long latency,
            ArrayList<String> reasons) {
        if (Boolean.TRUE.equals(AutonotifyProvider.SINGLETON.getConsiderLatency()) && AutonotifyProvider.SINGLETON.getLatencyThreshold() < latency)
            reasons.add("Latency threshold: " + AutonotifyProvider.SINGLETON.getLatencyThreshold() + "ms allowed vs. actually " + latency + "ms");
    }

    private static void checkExceptionsCriteria(Exception actualException, ArrayList<String> reasons) {
        var notificationSettings = AutonotifyProvider.SINGLETON;
        if (notificationSettings.getCustomExceptionsToConsider().contains(actualException.getClass()))
            reasons.add("An exception was thrown during its execution. It specifically matched one of the parameterized types set for notification (" + actualException.getClass().getSimpleName() + ")");
        else if (Autonotify.checkWhetherGenericallyMatchesMappedExceptions(notificationSettings, actualException))
            reasons.add("An exception was thrown during its execution. It generically matched one of the MappedException types set for notification (" + actualException.getClass().getSimpleName() + ")");
        else if (Boolean.TRUE.equals(notificationSettings.getConsiderUnexpectedExceptions()))
            reasons.add("An unexpected exception was thrown during its execution  (" + actualException.getClass().getSimpleName() + ")");
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
