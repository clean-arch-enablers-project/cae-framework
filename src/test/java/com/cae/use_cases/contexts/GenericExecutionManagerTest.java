package com.cae.use_cases.contexts;

import com.cae.mapped_exceptions.specifics.InternalMappedException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class GenericExecutionManagerTest {

    @Test
    @DisplayName("Should start with all properties null")
    void shouldStartWithAllPropertiesNull(){
        var newManager = new SomeManager();
        Assertions.assertNull(newManager.subject);
        Assertions.assertNull(newManager.exception);
        Assertions.assertNull(newManager.input);
        Assertions.assertNull(newManager.output);
    }

    @Test
    @DisplayName("Should init subject and tracking when calling the setSubjectAndStartTracking method")
    void shouldInitSubjectAndTrackingWhenCallingTheSetSubjectAndStartTrackingMethod(){
        var newManager = new SomeManager();
        Assertions.assertNull(newManager.subject);
        Assertions.assertFalse(newManager.hasStarted());
        var subject = "some-subject";
        newManager.setSubjectAndStartTracking(subject);
        Assertions.assertEquals(subject, newManager.subject);
        Assertions.assertTrue(newManager.hasStarted());
    }

    @Test
    @DisplayName("Should throw InternalMappedException if the instance has already been initialized and there is a reinitialization attempt")
    void shouldThrowInternalMappedExceptionIfTheInstanceHasAlreadyBeenInitializedAndThereIsAReinitializationAttempt(){
        var newManager = new SomeManager();
        newManager.setSubjectAndStartTracking("some-subject-again");
        Assertions.assertThrows(
                InternalMappedException.class,
                () -> newManager.setSubjectAndStartTracking("once-again-some-subject")
        );
    }

    @Test
    @DisplayName("Should be able to complete an instance as successful")
    void shouldBeAbleToCompleteAnInstanceAsSuccessful(){
        var newManager = new SomeManager();
        newManager.setSubjectAndStartTracking("another-subject");
        Assertions.assertFalse(newManager.isComplete());
        newManager.complete();
        Assertions.assertTrue(newManager.isComplete());
        Assertions.assertNull(newManager.exception);
        Assertions.assertTrue(newManager.wasSuccessful());
    }

    @Test
    @DisplayName("Should be able to complete an instance as unsuccessful")
    void shouldBeAbleToCompleteAnInstanceAsUnsuccessful(){
        var someProblem = new Exception("something went wrong...");
        var newManager = new SomeManager();
        newManager.setSubjectAndStartTracking("and-there-is-another-subject-again");
        Assertions.assertFalse(newManager.isComplete());
        newManager.complete(someProblem);
        Assertions.assertTrue(newManager.isComplete());
        Assertions.assertNotNull(newManager.exception);
        Assertions.assertEquals(someProblem, newManager.exception);
        Assertions.assertFalse(newManager.wasSuccessful());
    }

    @Test
    @DisplayName("Should throw InternalMappedException when asking whether successful and it hasn't completed yet")
    void shouldThrowInternalMappedExceptionWhenAskingWhetherSuccessfulAndItHasNotCompletedYet(){
        var newManager = new SomeManager();
        Assertions.assertThrows(InternalMappedException.class, newManager::wasSuccessful);
    }

    @Test
    @DisplayName("Should be a subtype of ExecutionTracker")
    void shouldBeSubtypeOfExecutionTracker(){
        Assertions.assertInstanceOf(ExecutionTracker.class, new SomeManager());
    }

    public static class SomeManager extends GenericExecutionManager{}

}
