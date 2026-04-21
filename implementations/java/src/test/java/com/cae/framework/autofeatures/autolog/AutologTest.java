package com.cae.framework.autofeatures.autolog;

import com.cae.context.ExecutionContext;
import com.cae.mapped_exceptions.specifics.InternalMappedException;
import com.cae.mapped_exceptions.specifics.NotFoundMappedException;
import lombok.Getter;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.MockedAutofeaturesRunnerProvider;

import java.util.List;
import java.util.UUID;

@ExtendWith(MockitoExtension.class)
public class AutologTest {

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
    void setup() {
        MockedAutofeaturesRunnerProvider.flushMockedInstances();
        AutologProvider.SINGLETON.reset();
        AutologProvider.SINGLETON.setProvidedInstance(this.logger);
    }

    @Test
    @DisplayName("Should call the provided instance's logInfo method")
    void shouldCallTheProvidedInstancesLogInfoMethod() throws InterruptedException {
        Mockito.verify(this.logger, Mockito.times(0)).logInfo(ArgumentMatchers.anyString());
        Autolog.runOn(successfulExec);
        Thread.sleep(1000);
        Mockito.verify(this.logger, Mockito.times(1)).logInfo(ArgumentMatchers.anyString());
    }

    @Test
    @DisplayName("Should call the provided instance's logError method")
    void shouldCallTheProvidedInstancesLogErrorMethod() throws InterruptedException {
        Mockito.verify(this.logger, Mockito.times(0)).logError(ArgumentMatchers.anyString());
        Autolog.runOn(unsuccessfulExec);
        Thread.sleep(1000);
        Mockito.verify(this.logger, Mockito.times(1)).logError(ArgumentMatchers.anyString());
    }

    @Test
    @DisplayName("If the provided instance is absent should throw InternalMappedException")
    void ifTheProvidedInstanceIsAbsentShouldThrowInternalMappedException() {
        AutologProvider.SINGLETON.setProvidedInstance(null);
        Assertions.assertThrows(InternalMappedException.class, () -> Autolog.runOn(successfulExec));
    }

    @Test
    @DisplayName("When structured format is ON should run without throwing")
    void whenStructuredFormatIsONShouldRunWithoutThrowing() {
        AutologProvider.SINGLETON.setStructuredFormat(true);
        Assertions.assertDoesNotThrow(() -> Autolog.writeBasedOn(successfulExec, this.logger));
        Assertions.assertDoesNotThrow(() -> Autolog.writeBasedOn(unsuccessfulExec, this.logger));
        Mockito.verify(this.logger, Mockito.times(1)).logInfo(ArgumentMatchers.anyString());
        Mockito.verify(this.logger, Mockito.times(1)).logError(ArgumentMatchers.anyString());
    }

    @Test
    @DisplayName("When structured format logging use case IO is ON should run without throwing")
    void whenStructuredFormatLoggingUseCaseIOIsONShouldRunWithoutThrowing() {
        AutologProvider.SINGLETON.setStructuredFormat(true).setSubjectsLoggingIO(true);
        Assertions.assertDoesNotThrow(() -> Autolog.writeBasedOn(successfulExec, this.logger));
        Assertions.assertDoesNotThrow(() -> Autolog.writeBasedOn(unsuccessfulExec, this.logger));
        Mockito.verify(this.logger, Mockito.times(1)).logInfo(ArgumentMatchers.anyString());
        Mockito.verify(this.logger, Mockito.times(1)).logError(ArgumentMatchers.anyString());
    }

    @Test
    @DisplayName("When structured format logging port IO is ON should run without throwing")
    void whenStructuredFormatLoggingPortIOIsONShouldRunWithoutThrowing() {
        AutologProvider.SINGLETON.setStructuredFormat(true).setInnerStepsLoggingIO(true);
        Assertions.assertDoesNotThrow(() -> Autolog.writeBasedOn(successfulExec, this.logger));
        Assertions.assertDoesNotThrow(() -> Autolog.writeBasedOn(unsuccessfulExec, this.logger));
        Mockito.verify(this.logger, Mockito.times(1)).logInfo(ArgumentMatchers.anyString());
        Mockito.verify(this.logger, Mockito.times(1)).logError(ArgumentMatchers.anyString());
    }

    @Test
    @DisplayName("When simple text format is ON should run without throwing")
    void whenSimpleTextFormatIsONShouldRunWithoutThrowing() {
        AutologProvider.SINGLETON.setStructuredFormat(false);
        Assertions.assertDoesNotThrow(() -> Autolog.writeBasedOn(successfulExec, this.logger));
        Assertions.assertDoesNotThrow(() -> Autolog.writeBasedOn(unsuccessfulExec, this.logger));
        Mockito.verify(this.logger, Mockito.times(1)).logInfo(ArgumentMatchers.anyString());
        Mockito.verify(this.logger, Mockito.times(1)).logError(ArgumentMatchers.anyString());
    }

    @Test
    @DisplayName("When simple text format logging use case IO is ON should run without throwing")
    void whenSimpleTextFormatLoggingUseCaseIOIsONShouldRunWithoutThrowing() {
        AutologProvider.SINGLETON.setStructuredFormat(false).setSubjectsLoggingIO(true);
        Assertions.assertDoesNotThrow(() -> Autolog.writeBasedOn(successfulExec, this.logger));
        Assertions.assertDoesNotThrow(() -> Autolog.writeBasedOn(unsuccessfulExec, this.logger));
        Mockito.verify(this.logger, Mockito.times(1)).logInfo(ArgumentMatchers.anyString());
        Mockito.verify(this.logger, Mockito.times(1)).logError(ArgumentMatchers.anyString());
    }

    @Test
    @DisplayName("When simple text format logging port IO is ON should run without throwing")
    void whenSimpleTextFormatLoggingPortIOIsONShouldRunWithoutThrowing() {
        AutologProvider.SINGLETON.setStructuredFormat(false).setInnerStepsLoggingIO(true);
        Assertions.assertDoesNotThrow(() -> Autolog.writeBasedOn(successfulExec, this.logger));
        Assertions.assertDoesNotThrow(() -> Autolog.writeBasedOn(unsuccessfulExec, this.logger));
        Mockito.verify(this.logger, Mockito.times(1)).logInfo(ArgumentMatchers.anyString());
        Mockito.verify(this.logger, Mockito.times(1)).logError(ArgumentMatchers.anyString());
    }

    @Test
    @DisplayName("Should run synchronously without throwing")
    void shouldRunSynchronouslyWithoutThrowing(){
        AutologProvider.SINGLETON.setAsync(false);
        Assertions.assertDoesNotThrow(() -> Autolog.runOn(successfulExec));
        AutologProvider.SINGLETON.reset();
    }

    static ExecutionContext setupSuccessfulExecution() throws InterruptedException {
        var exec = ExecutionContext.ofNew();
        exec.setSubjectAndStartTracking("AutologTesting", true);
        exec.setInput(new MainInput());
        Thread.sleep(50);
        var stepOne = exec.addStepInsightsOf("PortOne");
        stepOne.setInput(new PortOneInput());
        Thread.sleep(50);
        stepOne.setOutput(new PortOneOutput());
        stepOne.complete();
        var stepTwo = exec.addStepInsightsOf("PortTwo");
        Thread.sleep(334);
        stepTwo.complete();
        exec.setOutput(new MainOutput());
        exec.complete();
        return exec;
    }

    static ExecutionContext setupUnsuccessfulExecution() throws InterruptedException {
        var exec = ExecutionContext.ofNew();
        exec.setSubjectAndStartTracking("AutologTesting", true);
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

    @Getter
    public static class MainInput{
        private final List<Integer> numbers = List.of(1, 2, 3, 4, 5);
    }

    @Getter
    public static class MainOutput{
        private final String someField = UUID.randomUUID().toString();
    }

    @Getter
    public static class PortOneInput{
        private final String someInput = "some input here";
    }

    @Getter
    public static class PortOneOutput{
        private final Long number = 22L;
    }

}
