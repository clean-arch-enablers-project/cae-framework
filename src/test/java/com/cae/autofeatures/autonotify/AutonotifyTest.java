package com.cae.autofeatures.autonotify;

import com.cae.autofeatures.autolog.AutologProvider;
import com.cae.autofeatures.autolog.AutologTest;
import com.cae.autofeatures.autolog.Logger;
import com.cae.use_cases.contexts.ExecutionContext;
import org.junit.jupiter.api.*;
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
    @Mock
    Logger logger;

    static ExecutionContext successfulExec;

    @BeforeAll
    static void setupExecs() throws InterruptedException {
        successfulExec = setupSuccessfulExecution();
    }

    @BeforeEach
    void setup() {
        AutologProvider.SINGLETON.setProvidedInstance(this.logger);
        AutonotifyProvider.SINGLETON
                .considerLatency(500)
                .considerUnexpectedExceptions()
                .subscribe(this.subscriberOne)
                .subscribe(this.subscriberTwo);
    }

    @Test
    @DisplayName("Should dispatch the notification to every subscriber")
    void shouldDispatchTheNotificationToEverySubscriber() throws InterruptedException {
        Mockito.verify(this.subscriberOne, Mockito.times(0)).receiveNotification(ArgumentMatchers.any());
        Mockito.verify(this.subscriberTwo, Mockito.times(0)).receiveNotification(ArgumentMatchers.any());
        Autonotify.runOn(successfulExec);
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

    @Test
    @DisplayName("Should run synchronously without throwing")
    void shouldRunSynchronouslyWithoutThrowing(){
        AutonotifyProvider.SINGLETON.setAsync(false);
        Assertions.assertDoesNotThrow(() -> Autonotify.runOn(successfulExec));
        AutonotifyProvider.SINGLETON.reset();
    }

    static ExecutionContext setupSuccessfulExecution() throws InterruptedException {
        var exec = ExecutionContext.ofNew();
        exec.setSubjectAndStartTracking("AutologTesting");
        exec.setInput(new AutologTest.MainInput());
        Thread.sleep(250);
        var stepOne = exec.addStepInsightsOf("PortOne");
        stepOne.setInput(new AutologTest.PortOneInput());
        Thread.sleep(250);
        stepOne.setOutput(new AutologTest.PortOneOutput());
        stepOne.complete();
        var stepTwo = exec.addStepInsightsOf("PortTwo");
        Thread.sleep(10);
        stepTwo.complete();
        exec.setOutput(new AutologTest.MainOutput());
        exec.complete();
        return exec;
    }

}
