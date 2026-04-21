package com.cae.framework.autofeatures.autonotify;

import com.cae.env_vars.exceptions.MissingEnvVarException;
import com.cae.mapped_exceptions.specifics.*;
import com.cae.trier.retry.NoRetriesLeftException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.util.List;

@ExtendWith(MockitoExtension.class)
class AutonotifyProviderTest {

    @BeforeEach
    void setup(){
        AutonotifyProvider.SINGLETON.reset();
    }

    @Test
    @DisplayName("Default should be not to consider latency")
    void defaultShouldBeToNotConsiderLatency(){
        Assertions.assertFalse(AutonotifyProvider.SINGLETON.considerLatency);
    }

    @Test
    @DisplayName("Default should be not to consider unexpected exceptions")
    void defaultShouldBeNotToConsiderUnexpectedExceptions(){
        Assertions.assertFalse(AutonotifyProvider.SINGLETON.considerUnexpectedExceptions);
    }

    @Test
    @DisplayName("Default should be a null latency threshold")
    void defaultShouldBeANullLatencyThreshold(){
        Assertions.assertNull(AutonotifyProvider.SINGLETON.latencyThreshold);
    }

    @Test
    @DisplayName("Default should have an empty subscriber list")
    void defaultShouldHaveAnEmptySubscriberList(){
        Assertions.assertTrue(AutonotifyProvider.SINGLETON.subscribers.isEmpty());
    }

    @Test
    @DisplayName("Default should have an empty set of exceptions to consider")
    void defaultShouldHaveAnEmptySetOfExceptionsToConsider(){
        Assertions.assertTrue(AutonotifyProvider.SINGLETON.exceptionsToConsider.isEmpty());
    }

    @Test
    @DisplayName("When setting all subscribers at once, all of them should be available afterwards")
    void whenSettingAllSubscribersAtOnceAllOfThemShouldBeAvailableAfterwards(){
        var subscriberOne = Mockito.mock(AutonotifySubscriber.class);
        var subscriberTwo = Mockito.mock(AutonotifySubscriber.class);
        var subscriberThree = Mockito.mock(AutonotifySubscriber.class);
        var subscriberFour = Mockito.mock(AutonotifySubscriber.class);
        var allOfThem = List.of(subscriberOne, subscriberTwo, subscriberThree, subscriberFour);
        var provider = AutonotifyProvider.SINGLETON.setAllSubscribers(allOfThem);
        Assertions.assertEquals(AutonotifyProvider.SINGLETON, provider);
        var allWereSet = allOfThem.stream()
            .allMatch(subscriber -> provider.getSubscribers()
                .stream()
                .anyMatch(registeredSubscriber -> registeredSubscriber.equals(subscriber))
            );
        Assertions.assertTrue(allWereSet);
    }

    @Test
    @DisplayName("When setting one subscriber at a time, they should be available to use afterwards")
    void whenSettingOneSubscriberAtATimeTheyShouldBeAvailableToUseAfterwards(){
        var subscriberOne = Mockito.mock(AutonotifySubscriber.class);
        var provider = AutonotifyProvider.SINGLETON.subscribe(subscriberOne);
        Assertions.assertEquals(AutonotifyProvider.SINGLETON, provider);
        var isPresent = AutonotifyProvider.SINGLETON.getSubscribers()
                .stream()
                .anyMatch(subscriber -> subscriber.equals(subscriberOne));
        Assertions.assertTrue(isPresent);
    }

    @Test
    @DisplayName("When set to consider InputMappedException that type should be included in the exceptions to consider set")
    void whenSetToConsiderInputMappedExceptionThatTypeShouldBeIncludedInTheExceptionsToConsiderSet(){
        var wasNotIncludedBefore = AutonotifyProvider.SINGLETON.getExceptionsToConsider()
                .stream()
                .noneMatch(exceptionType -> exceptionType.equals(InputMappedException.class));
        var provider = AutonotifyProvider.SINGLETON.considerInputMappedExceptions();
        Assertions.assertEquals(AutonotifyProvider.SINGLETON, provider);
        var isIncludedAfter = AutonotifyProvider.SINGLETON.getExceptionsToConsider()
                .stream()
                .anyMatch(exceptionType -> exceptionType.equals(InputMappedException.class));
        Assertions.assertTrue(wasNotIncludedBefore);
        Assertions.assertTrue(isIncludedAfter);
    }

    @Test
    @DisplayName("When set to consider NotFoundMappedException that type should be included in the exceptions to consider set")
    void whenSetToConsiderNotFoundMappedExceptionThatTypeShouldBeIncludedInTheExceptionsToConsiderSet(){
        var wasNotIncludedBefore = AutonotifyProvider.SINGLETON.getExceptionsToConsider()
                .stream()
                .noneMatch(exceptionType -> exceptionType.equals(NotFoundMappedException.class));
        var provider = AutonotifyProvider.SINGLETON.considerNotFoundMappedExceptions();
        Assertions.assertEquals(AutonotifyProvider.SINGLETON, provider);
        var isIncludedAfter = AutonotifyProvider.SINGLETON.getExceptionsToConsider()
                .stream()
                .anyMatch(exceptionType -> exceptionType.equals(NotFoundMappedException.class));
        Assertions.assertTrue(wasNotIncludedBefore);
        Assertions.assertTrue(isIncludedAfter);
    }

    @Test
    @DisplayName("When set to consider NotAuthenticatedMappedException that type should be included in the exceptions to consider set")
    void whenSetToConsiderNotAuthenticatedMappedExceptionThatTypeShouldBeIncludedInTheExceptionsToConsiderSet(){
        var wasNotIncludedBefore = AutonotifyProvider.SINGLETON.getExceptionsToConsider()
                .stream()
                .noneMatch(exceptionType -> exceptionType.equals(NotAuthenticatedMappedException.class));
        var provider = AutonotifyProvider.SINGLETON.considerNotAuthenticatedMappedExceptions();
        Assertions.assertEquals(AutonotifyProvider.SINGLETON, provider);
        var isIncludedAfter = AutonotifyProvider.SINGLETON.getExceptionsToConsider()
                .stream()
                .anyMatch(exceptionType -> exceptionType.equals(NotAuthenticatedMappedException.class));
        Assertions.assertTrue(wasNotIncludedBefore);
        Assertions.assertTrue(isIncludedAfter);
    }

    @Test
    @DisplayName("When set to consider NotAuthorizedMappedException that type should be included in the exceptions to consider set")
    void whenSetToConsiderNotAuthorizedMappedExceptionThatTypeShouldBeIncludedInTheExceptionsToConsiderSet(){
        var wasNotIncludedBefore = AutonotifyProvider.SINGLETON.getExceptionsToConsider()
                .stream()
                .noneMatch(exceptionType -> exceptionType.equals(NotAuthorizedMappedException.class));
        var provider = AutonotifyProvider.SINGLETON.considerNotAuthorizedMappedExceptions();
        Assertions.assertEquals(AutonotifyProvider.SINGLETON, provider);
        var isIncludedAfter = AutonotifyProvider.SINGLETON.getExceptionsToConsider()
                .stream()
                .anyMatch(exceptionType -> exceptionType.equals(NotAuthorizedMappedException.class));
        Assertions.assertTrue(wasNotIncludedBefore);
        Assertions.assertTrue(isIncludedAfter);
    }

    @Test
    @DisplayName("When set to consider InternalMappedException that type should be included in the exceptions to consider set")
    void whenSetToConsiderInternalMappedExceptionThatTypeShouldBeIncludedInTheExceptionsToConsiderSet(){
        var wasNotIncludedBefore = AutonotifyProvider.SINGLETON.getExceptionsToConsider()
                .stream()
                .noneMatch(exceptionType -> exceptionType.equals(InternalMappedException.class));
        var provider = AutonotifyProvider.SINGLETON.considerInternalMappedExceptions();
        Assertions.assertEquals(AutonotifyProvider.SINGLETON, provider);
        var isIncludedAfter = AutonotifyProvider.SINGLETON.getExceptionsToConsider()
                .stream()
                .anyMatch(exceptionType -> exceptionType.equals(InternalMappedException.class));
        Assertions.assertTrue(wasNotIncludedBefore);
        Assertions.assertTrue(isIncludedAfter);
    }

    @Test
    @DisplayName("When set to consider NoRetriesLeftException that type should be included in the exceptions to consider set")
    void whenSetToConsiderNoRetriesLeftExceptionThatTypeShouldBeIncludedInTheExceptionsToConsiderSet(){
        var wasNotIncludedBefore = AutonotifyProvider.SINGLETON.getExceptionsToConsider()
                .stream()
                .noneMatch(exceptionType -> exceptionType.equals(NoRetriesLeftException.class));
        var provider = AutonotifyProvider.SINGLETON.considerNoRetriesLeftExceptions();
        Assertions.assertEquals(AutonotifyProvider.SINGLETON, provider);
        var isIncludedAfter = AutonotifyProvider.SINGLETON.getExceptionsToConsider()
                .stream()
                .anyMatch(exceptionType -> exceptionType.equals(NoRetriesLeftException.class));
        Assertions.assertTrue(wasNotIncludedBefore);
        Assertions.assertTrue(isIncludedAfter);
    }

    @Test
    @DisplayName("When set to consider MissingEnvVarException that type should be included in the exceptions to consider set")
    void whenSetToConsiderMissingEnvVarExceptionThatTypeShouldBeIncludedInTheExceptionsToConsiderSet(){
        var wasNotIncludedBefore = AutonotifyProvider.SINGLETON.getExceptionsToConsider()
                .stream()
                .noneMatch(exceptionType -> exceptionType.equals(MissingEnvVarException.class));
        var provider = AutonotifyProvider.SINGLETON.considerMissingEnvVarExceptions();
        Assertions.assertEquals(AutonotifyProvider.SINGLETON, provider);
        var isIncludedAfter = AutonotifyProvider.SINGLETON.getExceptionsToConsider()
                .stream()
                .anyMatch(exceptionType -> exceptionType.equals(MissingEnvVarException.class));
        Assertions.assertTrue(wasNotIncludedBefore);
        Assertions.assertTrue(isIncludedAfter);
    }

    @Test
    @DisplayName("Should be able to set specific types of exception to consider")
    void shouldBeAbleToSetSpecificTypesOfExceptionToConsider(){
        var exceptionsToConsiderBeforeSetup = AutonotifyProvider.SINGLETON.getExceptionsToConsider();
        var runtimeException = RuntimeException.class;
        var ioException = IOException.class;
        var illegalStateException = IllegalStateException.class;
        var runtimeExceptionWasNotIncluded = exceptionsToConsiderBeforeSetup.stream()
                .noneMatch(typeToConsider -> typeToConsider.equals(runtimeException));
        var ioExceptionWasNotIncluded = exceptionsToConsiderBeforeSetup.stream()
                .noneMatch(typeToConsider -> typeToConsider.equals(runtimeException));
        var illegalStateExceptionWasNotIncluded = exceptionsToConsiderBeforeSetup.stream()
                .noneMatch(typeToConsider -> typeToConsider.equals(runtimeException));
        AutonotifyProvider.SINGLETON.considerSpecifically(runtimeException)
                .considerSpecifically(ioException)
                .considerSpecifically(illegalStateException);
        var exceptionsToConsiderAfterSetup = AutonotifyProvider.SINGLETON.getExceptionsToConsider();
        var runtimeExceptionWasIncludedAfter = exceptionsToConsiderAfterSetup.stream()
                .anyMatch(typeToConsider -> typeToConsider.equals(runtimeException));
        var ioExceptionWasIncludedAfter = exceptionsToConsiderAfterSetup.stream()
                .anyMatch(typeToConsider -> typeToConsider.equals(runtimeException));
        var illegalStateExceptionWasIncludedAfter = exceptionsToConsiderAfterSetup.stream()
                .anyMatch(typeToConsider -> typeToConsider.equals(runtimeException));
        Assertions.assertTrue(runtimeExceptionWasNotIncluded);
        Assertions.assertTrue(ioExceptionWasNotIncluded);
        Assertions.assertTrue(illegalStateExceptionWasNotIncluded);
        Assertions.assertTrue(runtimeExceptionWasIncludedAfter);
        Assertions.assertTrue(ioExceptionWasIncludedAfter);
        Assertions.assertTrue(illegalStateExceptionWasIncludedAfter);
    }

    @Test
    @DisplayName("Should set async as expected")
    void shouldSetAsyncAsExpected(){
        var provider = AutonotifyProvider.SINGLETON.setAsync(false);
        Assertions.assertEquals(AutonotifyProvider.SINGLETON, provider);
        Assertions.assertFalse(AutonotifyProvider.SINGLETON.getAsync());
    }

}
