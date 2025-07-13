package com.cae.autofeatures.autonotify;

import com.cae.autofeatures.autolog.native_io_extraction_mode.json.SimpleJsonBuilder;
import com.cae.env_vars.exceptions.MissingEnvVarException;
import com.cae.mapped_exceptions.specifics.*;
import com.cae.trier.retry.NoRetriesLeftException;
import com.cae.use_cases.contexts.ExecutionContext;
import com.cae.use_cases.contexts.GenericExecutionManager;
import lombok.Builder;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;

@Builder
@Getter
public class Notification{

    public static List<Notification> createNewOnesBasedOn(ExecutionContext executionContext){
        var newNotifications = new ArrayList<Notification>();
        var mainNotification = Notification.generateBasedOn(executionContext, executionContext.getCorrelationId());
        mainNotification.ifPresent(newNotifications::add);
        executionContext.getStepInsights()
                .stream()
                .map(step -> Notification.generateBasedOn(step, executionContext.getCorrelationId()))
                .filter(Optional::isPresent)
                .map(Optional::get)
                .forEach(newNotifications::add);
        return newNotifications;
    }

    private static Optional<Notification> generateBasedOn(GenericExecutionManager execManager, UUID correlationId) {
        var reasons = new ArrayList<String>();
        Notification.validateLatencyCriteria(execManager, reasons);
        Notification.validateExceptionCriteria(execManager, reasons);
        if (reasons.isEmpty())
            return Optional.empty();
        return Optional.of(Notification.builder()
                .correlationId(correlationId)
                .reasons(reasons)
                .subject(execManager.getSubject())
                .exception(execManager.getException())
                .build());
    }

    protected static void validateLatencyCriteria(GenericExecutionManager execManager, ArrayList<String> reasons) {
        boolean oughtToConsiderLatency = AutonotifyProvider.SINGLETON.getConsiderLatency();
        var latencyThreshold = AutonotifyProvider.SINGLETON.getLatencyThreshold();
        var actualLatency = execManager.calculateLatency();
        if (oughtToConsiderLatency && latencyThreshold < actualLatency)
            reasons.add("Latency threshold: " + latencyThreshold + "ms allowed vs. actually " + actualLatency + "ms");
    }

    protected static void validateExceptionCriteria(GenericExecutionManager execManager, ArrayList<String> reasons) {
        if (!execManager.wasSuccessful()){
            var providedSetup = AutonotifyProvider.SINGLETON;
            var exceptionClass = execManager.getException().getClass();
            boolean oughtToConsiderUnexpectedExceptions = providedSetup.getConsiderUnexpectedExceptions();
            if (providedSetup.getExceptionsToConsider().contains(exceptionClass))
                reasons.add("An exception was thrown during its execution. It specifically matched one of the parameterized types set for notification (" + exceptionClass.getSimpleName() + ")");
            else if (Notification.checkWhetherGenericallyMatchesMappedExceptions(providedSetup, execManager.getException()))
                reasons.add("An exception was thrown during its execution. It generically matched one of the MappedException types set for notification (" + exceptionClass.getSimpleName() + ")");
            else if (oughtToConsiderUnexpectedExceptions)
                reasons.add("An unexpected exception was thrown during its execution  (" + exceptionClass.getSimpleName() + ")");
        }
    }

    protected static boolean checkWhetherGenericallyMatchesMappedExceptions(
            AutonotifyProvider providedSetup,
            Exception exception) {
        var map = providedSetup.getExceptionsToConsider();
        return Stream.of(
                InternalMappedException.class,
                InputMappedException.class,
                NotFoundMappedException.class,
                NotAuthenticatedMappedException.class,
                NotAuthorizedMappedException.class,
                NoRetriesLeftException.class,
                MissingEnvVarException.class
        ).anyMatch(type -> map.contains(type) && type.isInstance(exception));
    }

    protected final String subject;
    protected final UUID correlationId;
    protected final Exception exception;
    protected final List<String> reasons;

    @Override
    public String toString() {
        return "Notification generated on '" +
                this.subject +"' during the execution of correlation ID '" +
                this.correlationId.toString() + "' because of the following reasons: " +
                SimpleJsonBuilder.buildFor(this.reasons);
    }
}
