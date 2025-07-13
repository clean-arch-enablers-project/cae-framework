package com.cae.autofeatures.autometrics;

import com.cae.autofeatures.autolog.AutologProvider;
import com.cae.autofeatures.autolog.AutologTest;
import com.cae.autofeatures.autolog.Logger;
import com.cae.mapped_exceptions.specifics.NotFoundMappedException;
import com.cae.use_cases.contexts.ExecutionContext;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.MockedAutofeaturesRunnerProvider;


@ExtendWith(MockitoExtension.class)
class AutometricsTest {

    @Mock
    AutometricsSubscriber subscriberOne;
    @Mock
    AutometricsSubscriber subscriberTwo;
    @Mock
    Logger logger;

    static ExecutionContext successfulExec;
    static ExecutionContext unsuccessfulExec;

    @BeforeAll
    static void setupExecs() throws InterruptedException {
        successfulExec = setupSuccessfulExecution();
        unsuccessfulExec = setupUnsuccessfulExecution();
    }

    @BeforeEach
    void setup(){
        MockedAutofeaturesRunnerProvider.flushMockedInstances();
        AutologProvider.SINGLETON.setProvidedInstance(this.logger);
    }

    @Test
    @DisplayName("Should call all subscribers")
    void shouldCallAllSubscribers() throws InterruptedException {
        AutometricsProvider.SINGLETON.subscribe(this.subscriberOne).subscribe(this.subscriberTwo);
        Mockito.verify(this.subscriberOne, Mockito.times(0)).receiveMetric(ArgumentMatchers.any());
        Mockito.verify(this.subscriberTwo, Mockito.times(0)).receiveMetric(ArgumentMatchers.any());
        Autometrics.runOn(successfulExec);
        Thread.sleep(100);
        Mockito.verify(this.subscriberOne, Mockito.times(3)).receiveMetric(ArgumentMatchers.any());
        Mockito.verify(this.subscriberTwo, Mockito.times(3)).receiveMetric(ArgumentMatchers.any());
        AutometricsProvider.SINGLETON.reset();
    }

    @Test
    @DisplayName("Should be able to manually dispatch the metric to all subscribers")
    void shouldBeAbleToManuallyDispatchTheMetricToAllSubscribers(){
        AutometricsProvider.SINGLETON.subscribe(this.subscriberOne).subscribe(this.subscriberTwo);
        Mockito.verify(this.subscriberOne, Mockito.times(0)).receiveMetric(ArgumentMatchers.any());
        Mockito.verify(this.subscriberTwo, Mockito.times(0)).receiveMetric(ArgumentMatchers.any());
        var metric = Mockito.mock(Metric.class);
        Autometrics.send(metric);
        Mockito.verify(this.subscriberOne, Mockito.times(1)).receiveMetric(ArgumentMatchers.any());
        Mockito.verify(this.subscriberTwo, Mockito.times(1)).receiveMetric(ArgumentMatchers.any());
        AutometricsProvider.SINGLETON.reset();
    }

    @Test
    @DisplayName("Should run synchronously without throwing")
    void shouldRunSynchronouslyWithoutThrowing(){
        AutometricsProvider.SINGLETON.setAsync(false);
        Assertions.assertDoesNotThrow(() -> Autometrics.runOn(successfulExec));
        AutometricsProvider.SINGLETON.reset();
    }

    static ExecutionContext setupSuccessfulExecution() throws InterruptedException {
        var exec = ExecutionContext.ofNew();
        exec.setSubjectAndStartTracking("AutologTesting");
        exec.setInput(new AutologTest.MainInput());
        Thread.sleep(50);
        var stepOne = exec.addStepInsightsOf("PortOne");
        stepOne.setInput(new AutologTest.PortOneInput());
        Thread.sleep(50);
        stepOne.setOutput(new AutologTest.PortOneOutput());
        stepOne.complete();
        var stepTwo = exec.addStepInsightsOf("PortTwo");
        Thread.sleep(334);
        stepTwo.complete();
        exec.setOutput(new AutologTest.MainOutput());
        exec.complete();
        return exec;
    }

    static ExecutionContext setupUnsuccessfulExecution() throws InterruptedException {
        var exec = ExecutionContext.ofNew();
        exec.setSubjectAndStartTracking("AutologTesting");
        Thread.sleep(50);
        var stepOne = exec.addStepInsightsOf("PortOne");
        Thread.sleep(50);
        stepOne.complete();
        var stepTwo = exec.addStepInsightsOf("PortTwo");
        Thread.sleep(334);
        stepTwo.complete();
        exec.complete(new NotFoundMappedException(""));
        return exec;
    }

}
