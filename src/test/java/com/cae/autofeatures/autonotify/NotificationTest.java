package com.cae.autofeatures.autonotify;

import com.cae.autofeatures.autolog.native_io_extraction_mode.json.SimpleJsonBuilder;
import com.cae.env_vars.exceptions.MissingEnvVarException;
import com.cae.http_client.implementations.exceptions.IORuntimeException;
import com.cae.mapped_exceptions.specifics.*;
import com.cae.trier.autoretry.NoRetriesLeftException;
import com.cae.trier.autoretry.OnExhaustion;
import com.cae.use_cases.contexts.ExecutionContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.util.ArrayList;

@ExtendWith(MockitoExtension.class)
class NotificationTest {

    @BeforeEach
    void setup(){
        AutonotifyProvider.SINGLETON.reset();
    }

    @Test
    @DisplayName("Should increment reasons array for latency criteria")
    void shouldIncrementReasonsArrayForLatencyCriteria() throws InterruptedException {
        var step = ExecutionContext.StepInsight.of("testing");
        var latencyThreshold = 500;
        var badLatency = 2000;
        Thread.sleep(badLatency);
        step.complete();
        AutonotifyProvider.SINGLETON.considerLatency(latencyThreshold);
        var reasons = new ArrayList<String>();
        Notification.validateLatencyCriteria(step, reasons);
        Assertions.assertFalse(reasons.isEmpty());
    }

    @Test
    @DisplayName("Should not increment reasons array for latency criteria")
    void shouldNotIncrementReasonsArrayForLatencyCriteria() throws InterruptedException {
        var step = ExecutionContext.StepInsight.of("testing");
        var latencyThreshold = 500;
        var goodLatency = 300;
        Thread.sleep(goodLatency);
        step.complete();
        AutonotifyProvider.SINGLETON.considerLatency(latencyThreshold);
        var reasons = new ArrayList<String>();
        Notification.validateLatencyCriteria(step, reasons);
        Assertions.assertTrue(reasons.isEmpty());
    }

    @Test
    @DisplayName("Should not increment reasons array for exception criteria when exec is successful")
    void shouldNotIncrementReasonsArrayForExceptionCriteriaWhenExecIsSuccessful() throws InterruptedException {
        var step = ExecutionContext.StepInsight.of("testing");
        Thread.sleep(100);
        step.complete();
        AutonotifyProvider.SINGLETON.considerSpecifically(IORuntimeException.class);
        var reasons = new ArrayList<String>();
        Notification.validateExceptionCriteria(step, reasons);
        Assertions.assertTrue(reasons.isEmpty());
    }

    @Test
    @DisplayName("Should increment reasons array for exception criteria when matches specifically")
    void shouldIncrementReasonsArrayForExceptionCriteriaWhenMatchesSpecifically() throws InterruptedException {
        var step = ExecutionContext.StepInsight.of("testing");
        Thread.sleep(100);
        step.complete(new IORuntimeException(new IOException("ouch!")));
        AutonotifyProvider.SINGLETON.considerSpecifically(IORuntimeException.class);
        var reasons = new ArrayList<String>();
        Notification.validateExceptionCriteria(step, reasons);
        Assertions.assertFalse(reasons.isEmpty());
    }

    @Test
    @DisplayName("Should not increment reasons array for exception criteria when matches specifically")
    void shouldNotIncrementReasonsArrayForExceptionCriteriaWhenMatchesSpecifically() throws InterruptedException {
        var step = ExecutionContext.StepInsight.of("testing");
        Thread.sleep(100);
        step.complete(new IORuntimeException(new IOException("ouch!")));
        var reasons = new ArrayList<String>();
        Notification.validateExceptionCriteria(step, reasons);
        Assertions.assertTrue(reasons.isEmpty());
    }

    @Test
    @DisplayName("Should increment reasons array for exception criteria when matches generically InternalMappedException")
    void shouldIncrementReasonsArrayForExceptionCriteriaWhenMatchesGenericallyInternalMappedException() throws InterruptedException {
        var step = ExecutionContext.StepInsight.of("testing");
        Thread.sleep(100);
        step.complete(new InternalMappedExceptionSubtype());
        AutonotifyProvider.SINGLETON.considerInternalMappedExceptions();
        var reasons = new ArrayList<String>();
        Notification.validateExceptionCriteria(step, reasons);
        Assertions.assertFalse(reasons.isEmpty());
    }

    @Test
    @DisplayName("Should increment reasons array for exception criteria when matches generically InputMappedException")
    void shouldIncrementReasonsArrayForExceptionCriteriaWhenMatchesGenericallyInputMappedException() throws InterruptedException {
        var step = ExecutionContext.StepInsight.of("testing");
        Thread.sleep(100);
        step.complete(new InputMappedExceptionSubtype());
        AutonotifyProvider.SINGLETON.considerInputMappedExceptions();
        var reasons = new ArrayList<String>();
        Notification.validateExceptionCriteria(step, reasons);
        Assertions.assertFalse(reasons.isEmpty());
    }

    @Test
    @DisplayName("Should increment reasons array for exception criteria when matches generically NotFoundMappedException")
    void shouldIncrementReasonsArrayForExceptionCriteriaWhenMatchesGenericallyNotFoundMappedException() throws InterruptedException {
        var step = ExecutionContext.StepInsight.of("testing");
        Thread.sleep(100);
        step.complete(new NotFoundMappedExceptionSubtype());
        AutonotifyProvider.SINGLETON.considerNotFoundMappedExceptions();
        var reasons = new ArrayList<String>();
        Notification.validateExceptionCriteria(step, reasons);
        Assertions.assertFalse(reasons.isEmpty());
    }

    @Test
    @DisplayName("Should increment reasons array for exception criteria when matches generically NotAuthenticatedMappedException")
    void shouldIncrementReasonsArrayForExceptionCriteriaWhenMatchesGenericallyNotAuthenticatedMappedException() throws InterruptedException {
        var step = ExecutionContext.StepInsight.of("testing");
        Thread.sleep(100);
        step.complete(new NotAuthenticatedMappedExceptionSubtype());
        AutonotifyProvider.SINGLETON.considerNotAuthenticatedMappedExceptions();
        var reasons = new ArrayList<String>();
        Notification.validateExceptionCriteria(step, reasons);
        Assertions.assertFalse(reasons.isEmpty());
    }

    @Test
    @DisplayName("Should increment reasons array for exception criteria when matches generically NotAuthorizedMappedException")
    void shouldIncrementReasonsArrayForExceptionCriteriaWhenMatchesGenericallyNotAuthorizedMappedException() throws InterruptedException {
        var step = ExecutionContext.StepInsight.of("testing");
        Thread.sleep(100);
        step.complete(new NotAuthorizedMappedExceptionSubtype());
        AutonotifyProvider.SINGLETON.considerNotAuthorizedMappedExceptions();
        var reasons = new ArrayList<String>();
        Notification.validateExceptionCriteria(step, reasons);
        Assertions.assertFalse(reasons.isEmpty());
    }

    @Test
    @DisplayName("Should increment reasons array for exception criteria when matches generically NoRetriesLeftException")
    void shouldIncrementReasonsArrayForExceptionCriteriaWhenMatchesGenericallyNoRetriesLeftException() throws InterruptedException {
        var step = ExecutionContext.StepInsight.of("testing");
        Thread.sleep(100);
        step.complete(new NoRetriesLeftMappedExceptionSubtype());
        AutonotifyProvider.SINGLETON.considerNoRetriesLeftExceptions();
        var reasons = new ArrayList<String>();
        Notification.validateExceptionCriteria(step, reasons);
        Assertions.assertFalse(reasons.isEmpty());
    }

    @Test
    @DisplayName("Should increment reasons array for exception criteria when matches generically MissingEnvVarException")
    void shouldIncrementReasonsArrayForExceptionCriteriaWhenMatchesGenericallyMissingEnvVarException() throws InterruptedException {
        var step = ExecutionContext.StepInsight.of("testing");
        Thread.sleep(100);
        step.complete(new MissingEnvVarExceptionSubtype());
        AutonotifyProvider.SINGLETON.considerMissingEnvVarExceptions();
        var reasons = new ArrayList<String>();
        Notification.validateExceptionCriteria(step, reasons);
        Assertions.assertFalse(reasons.isEmpty());
    }

    @Test
    @DisplayName("Should not increment reasons array for exception criteria when matches generically")
    void shouldNotIncrementReasonsArrayForExceptionCriteriaWhenMatchesGenerically() throws InterruptedException {
        var step = ExecutionContext.StepInsight.of("testing");
        Thread.sleep(100);
        step.complete(new InternalMappedExceptionSubtype());
        var reasons = new ArrayList<String>();
        Notification.validateExceptionCriteria(step, reasons);
        Assertions.assertTrue(reasons.isEmpty());
    }

    @Test
    @DisplayName("Should increment reasons array for exception criteria when matches unexpectedly")
    void shouldIncrementReasonsArrayForExceptionCriteriaWhenMatchesUnexpectedly() throws InterruptedException {
        var step = ExecutionContext.StepInsight.of("testing");
        Thread.sleep(100);
        step.complete(new Exception("ouch again!"));
        AutonotifyProvider.SINGLETON.considerUnexpectedExceptions();
        var reasons = new ArrayList<String>();
        Notification.validateExceptionCriteria(step, reasons);
        Assertions.assertFalse(reasons.isEmpty());
    }

    @Test
    @DisplayName("Should not increment reasons array for exception criteria when matches unexpectedly")
    void shouldNotIncrementReasonsArrayForExceptionCriteriaWhenMatchesUnexpectedly() throws InterruptedException {
        var step = ExecutionContext.StepInsight.of("testing");
        Thread.sleep(100);
        step.complete(new Exception("ouch again!"));
        var reasons = new ArrayList<String>();
        Notification.validateExceptionCriteria(step, reasons);
        Assertions.assertTrue(reasons.isEmpty());
    }

    @Test
    @DisplayName("Should create notifications as expected")
    void shouldCreateNotificationsAsExpected() throws InterruptedException {
        var execContext = ExecutionContext.ofNew();
        execContext.setSubjectAndStartTracking("AutonotifyTests");
        var stepOne = execContext.addStepInsightsOf("step1");
        Thread.sleep(50);
        stepOne.complete();
        var stepTwo = execContext.addStepInsightsOf("step2");
        Thread.sleep(100);
        stepTwo.complete();
        var slowStepThree = execContext.addStepInsightsOf("slowStep3");
        Thread.sleep(5000);
        slowStepThree.complete();
        execContext.complete(new NotFoundMappedException("ops!"));
        AutonotifyProvider.SINGLETON.considerNotFoundMappedExceptions().considerLatency(2000);
        var notifications = Notification.createNewOnesBasedOn(execContext);
        Assertions.assertEquals(2, notifications.size());
        var oneNotificationIsForSlowStepThree = notifications.stream()
                .anyMatch(notification -> notification.getSubject().equals(slowStepThree.getSubject()));
        var anotherNotificationIsForNotFoundMappedException = notifications
                .stream()
                .anyMatch(notification -> notification.getSubject().equals(execContext.getSubject()));
        Assertions.assertTrue(oneNotificationIsForSlowStepThree);
        Assertions.assertTrue(anotherNotificationIsForNotFoundMappedException);
    }

    @Test
    @DisplayName("Should preserve correlation ID")
    void shouldPreserveCorrelationId() throws InterruptedException {
        var execContext = ExecutionContext.ofNew();
        execContext.setSubjectAndStartTracking("AutonotifyTests");
        var stepOne = execContext.addStepInsightsOf("step1");
        Thread.sleep(1000);
        stepOne.complete();
        execContext.complete();
        AutonotifyProvider.SINGLETON.considerLatency(500);
        var notifications = Notification.createNewOnesBasedOn(execContext);
        var allNotificationsHaveTheSameCorrelationId = notifications.stream()
                .allMatch(notification -> notification.getCorrelationId().equals(execContext.getCorrelationId()));
        Assertions.assertTrue(allNotificationsHaveTheSameCorrelationId);
    }

    @Test
    @DisplayName("Should preserve exception")
    void shouldPreserveException() throws InterruptedException {
        var execContext = ExecutionContext.ofNew();
        execContext.setSubjectAndStartTracking("Testing");
        Thread.sleep(10);
        execContext.complete(new MissingEnvVarException("ops!"));
        AutonotifyProvider.SINGLETON.considerMissingEnvVarExceptions();
        var notifications = Notification.createNewOnesBasedOn(execContext);
        Assertions.assertEquals(execContext.getException(), notifications.get(0).getException());
    }

    @Test
    @DisplayName("Notifications should have their reasons")
    void notificationsShouldHaveTheirReasons() throws InterruptedException {
        var execContext = ExecutionContext.ofNew();
        execContext.setSubjectAndStartTracking("Testing");
        Thread.sleep(10);
        execContext.complete(new MissingEnvVarException("ops!"));
        AutonotifyProvider.SINGLETON.considerMissingEnvVarExceptions().considerLatency(5);
        var notifications = Notification.createNewOnesBasedOn(execContext);
        var allNotificationsHaveTheirReasons = notifications.stream()
                .noneMatch(notification -> notification.getReasons().isEmpty());
        Assertions.assertTrue(allNotificationsHaveTheirReasons);
    }

    @Test
    @DisplayName("toString should be as expected")
    void toStringShouldBeAsExpected() throws InterruptedException {
        var execContext = ExecutionContext.ofNew();
        execContext.setSubjectAndStartTracking("Testing");
        Thread.sleep(10);
        execContext.complete(new MissingEnvVarException("ops!"));
        AutonotifyProvider.SINGLETON.considerMissingEnvVarExceptions();
        var notification = Notification.createNewOnesBasedOn(execContext).get(0);
        var expected = "Notification generated on '" +
                notification.subject +"' during the execution of correlation ID '" +
                notification.correlationId.toString() + "' because of the following reasons: " +
                SimpleJsonBuilder.buildFor(notification.reasons);
        Assertions.assertEquals(expected, notification.toString());
    }

    public static class InternalMappedExceptionSubtype extends InternalMappedException{
        public InternalMappedExceptionSubtype() {
            super("some testing with InternalMappedExceptionSubtype", "InternalMappedExceptionSubtype happening here");
        }
    }

    public static class InputMappedExceptionSubtype extends InputMappedException {
        public InputMappedExceptionSubtype() {
            super("some testing with InputMappedExceptionSubtype", " InputMappedExceptionSubtype happening here");
        }
    }

    public static class NotFoundMappedExceptionSubtype extends NotFoundMappedException {
        public NotFoundMappedExceptionSubtype() {
            super("some testing with NotFoundMappedExceptionSubtype", " NotFoundMappedExceptionSubtype happening here");
        }
    }

    public static class NotAuthenticatedMappedExceptionSubtype extends NotAuthenticatedMappedException {
        public NotAuthenticatedMappedExceptionSubtype() {
            super("some testing with NotAuthenticatedMappedExceptionSubtype", " NotAuthenticatedMappedExceptionSubtype happening here");
        }
    }

    public static class NotAuthorizedMappedExceptionSubtype extends NotAuthorizedMappedException {
        public NotAuthorizedMappedExceptionSubtype() {
            super("some testing with NotAuthorizedMappedExceptionSubtype", " NotAuthorizedMappedExceptionSubtype happening here");
        }
    }

    public static class NoRetriesLeftMappedExceptionSubtype extends NoRetriesLeftException {
        public NoRetriesLeftMappedExceptionSubtype() {
            super("some testing with NoRetriesLeftMappedExceptionSubtype", Mockito.mock(OnExhaustion.FailureStatus.class));
        }
    }

    public static class MissingEnvVarExceptionSubtype extends MissingEnvVarException {
        public MissingEnvVarExceptionSubtype() {
            super("some testing with MissingEnvVarExceptionSubtype");
        }
    }



}
