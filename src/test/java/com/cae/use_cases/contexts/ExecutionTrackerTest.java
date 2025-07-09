package com.cae.use_cases.contexts;

import com.cae.mapped_exceptions.specifics.InternalMappedException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ExecutionTrackerTest {

    @Test
    @DisplayName("When just-instantiated should have all of its properties null")
    void whenJustInstantiatedShouldHaveAllOfItsPropertiesNull(){
        var newTracker = new SomeExecutionTracker();
        Assertions.assertNull(newTracker.startTime);
        Assertions.assertNull(newTracker.endTime);
        Assertions.assertNull(newTracker.latency);
    }

    @Test
    @DisplayName("Should throw InternalMappedException when trying to end tracking of a not initiated instance")
    void shouldThrowInternalMappedExceptionWhenTryingToEndTrackingOfANotInitiatedInstance(){
        var newTracker = new SomeExecutionTracker();
        Assertions.assertThrows(InternalMappedException.class, newTracker::endTracking);
    }

    @Test
    @DisplayName("When just-instantiated should be able to start tracking")
    void whenJustInstantiatedShouldBeAbleToStartTracking(){
        var newTracker = new SomeExecutionTracker();
        Assertions.assertNull(newTracker.getStartTime());
        Assertions.assertDoesNotThrow(newTracker::startTracking);
        Assertions.assertNotNull(newTracker.getStartTime());
    }

    @Test
    @DisplayName("When the instance has been initiated should return TRUE as to indicating the instance has started")
    void whenTheInstanceHasBeenInitiatedShouldReturnTrueAsToIndicatingTheInstanceHasStarted(){
        var newTracker = new SomeExecutionTracker();
        Assertions.assertFalse(newTracker.hasStarted());
        newTracker.startTracking();
        Assertions.assertTrue(newTracker.hasStarted());
    }

    @Test
    @DisplayName("Should throw when trying to initiate an instance already initiated")
    void shouldThrowWhenTryingToInitiateAnInstanceAlreadyInitiated(){
        var newTracker = new SomeExecutionTracker();
        newTracker.startTracking();
        Assertions.assertThrows(InternalMappedException.class, newTracker::startTracking);
    }

    @Test
    @DisplayName("When just-initiated should be able to end tracking")
    void whenJustInitiatedShouldBeAbleToEndTracking(){
        var newTracker = new SomeExecutionTracker();
        newTracker.startTracking();
        Assertions.assertNull(newTracker.getEndTime());
        Assertions.assertDoesNotThrow(newTracker::endTracking);
        Assertions.assertNotNull(newTracker.getEndTime());
    }

    @Test
    @DisplayName("Should return TRUE as to indicating the instance has ended")
    void shouldReturnTrueAsToIndicatingTheInstanceHasEnded(){
        var newTracker = new SomeExecutionTracker();
        newTracker.startTracking();
        Assertions.assertFalse(newTracker.isComplete());
        newTracker.endTracking();
        Assertions.assertTrue(newTracker.isComplete());
    }

    @Test
    @DisplayName("Should throw InternalMappedException when trying to get latency before the instance completion")
    void shouldThrowInternalMappedExceptionWhenTryingToGetLatencyBeforeTheInstanceCompletion(){
        var newTracker = new SomeExecutionTracker();
        Assertions.assertThrows(InternalMappedException.class, newTracker::getLatency);
    }

    @Test
    @DisplayName("Should be able to return the latency when the instance has completed")
    void shouldBeAbleToReturnTheLatencyWhenTheInstanceHasCompleted() throws InterruptedException {
        var newTracker = new SomeExecutionTracker();
        newTracker.startTracking();
        Thread.sleep(2000);
        newTracker.endTracking();
        var latency = newTracker.getLatency();
        Assertions.assertTrue((2000 <= latency && latency < 3000));
    }

    public static class SomeExecutionTracker extends ExecutionTracker{}

}
