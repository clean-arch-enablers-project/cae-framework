package com.cae.framework.autofeatures.autometrics;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class AutometricsProviderTest {

    @BeforeEach
    void setup(){
        AutometricsProvider.SINGLETON.reset();
    }

    @Test
    @DisplayName("Should add subscribers as expected")
    void shouldAddSubscribersAsExpected(){
        var subscriberOne = Mockito.mock(AutometricsSubscriber.class);
        var subscriberTwo = Mockito.mock(AutometricsSubscriber.class);
        var subscriberOneWasNotPresent = AutometricsProvider.SINGLETON.getSubscribers()
                .stream()
                .noneMatch(subscriber -> subscriber.equals(subscriberOne));
        var subscriberTwoWasNotPresent = AutometricsProvider.SINGLETON.getSubscribers()
                .stream()
                .noneMatch(subscriber -> subscriber.equals(subscriberTwo));
        AutometricsProvider.SINGLETON.subscribe(subscriberOne).subscribe(subscriberTwo);
        var subscriberOneIsPresent = AutometricsProvider.SINGLETON.getSubscribers()
                .stream()
                .anyMatch(subscriber -> subscriber.equals(subscriberOne));
        var subscriberTwoIsPresent = AutometricsProvider.SINGLETON.getSubscribers()
                .stream()
                .anyMatch(subscriber -> subscriber.equals(subscriberTwo));
        Assertions.assertTrue(subscriberOneWasNotPresent);
        Assertions.assertTrue(subscriberTwoWasNotPresent);
        Assertions.assertTrue(subscriberOneIsPresent);
        Assertions.assertTrue(subscriberTwoIsPresent);
    }

    @Test
    @DisplayName("Should set async as expected")
    void shouldSetAsyncAsExpected(){
        var provider = AutometricsProvider.SINGLETON.setAsync(false);
        Assertions.assertEquals(AutometricsProvider.SINGLETON, provider);
        Assertions.assertFalse(AutometricsProvider.SINGLETON.getAsync());
    }

}
