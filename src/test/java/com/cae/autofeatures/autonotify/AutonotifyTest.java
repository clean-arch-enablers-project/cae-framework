package com.cae.autofeatures.autonotify;

import com.cae.use_cases.contexts.ExecutionContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class AutonotifyTest {

    @Mock
    AutonotifySubscriber subscriberOne;

    @Mock
    AutonotifySubscriber subscriberTwo;

    ExecutionContext executionContext;

    @BeforeEach
    void setup() throws InterruptedException {
        this.executionContext = ExecutionContext.ofNew();
        this.executionContext.setSubjectAndStartTracking("AutonotifyTesting");
        Thread.sleep(2000);
        this.executionContext.complete(new Exception("Ops..."));
        AutonotifyProvider.SINGLETON
                .considerLatency(1000)
                .considerUnexpectedExceptions()
                .setSubscriber(this.subscriberOne)
                .setSubscriber(this.subscriberTwo);
    }

    @Test
    @DisplayName("Should dispatch the notification to every subscriber")
    void shouldDispatchTheNotificationToEverySubscriber() throws InterruptedException {
        Mockito.verify(this.subscriberOne, Mockito.times(0)).receiveNotification(ArgumentMatchers.any());
        Mockito.verify(this.subscriberTwo, Mockito.times(0)).receiveNotification(ArgumentMatchers.any());
        Autonotify.runOn(executionContext);
        Thread.sleep(1000);
        Mockito.verify(this.subscriberOne, Mockito.times(1)).receiveNotification(ArgumentMatchers.any());
        Mockito.verify(this.subscriberTwo, Mockito.times(1)).receiveNotification(ArgumentMatchers.any());
        AutonotifyProvider.SINGLETON.reset();
    }

    @Test
    @DisplayName("Should send notification manually")
    void shouldSendNotificationManually(){
        var notification = Mockito.mock(Notification.class);
        Mockito.verify(this.subscriberOne, Mockito.times(0)).receiveNotification(ArgumentMatchers.any());
        Mockito.verify(this.subscriberTwo, Mockito.times(0)).receiveNotification(ArgumentMatchers.any());
        Autonotify.send(notification);
        Mockito.verify(this.subscriberOne, Mockito.times(1)).receiveNotification(ArgumentMatchers.any());
        Mockito.verify(this.subscriberTwo, Mockito.times(1)).receiveNotification(ArgumentMatchers.any());
        AutonotifyProvider.SINGLETON.reset();
    }

}
