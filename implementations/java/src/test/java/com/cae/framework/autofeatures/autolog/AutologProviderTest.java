package com.cae.framework.autofeatures.autolog;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class AutologProviderTest {

    @BeforeEach
    void setup(){
        AutologProvider.SINGLETON.reset();
    }

    @Test
    @DisplayName("Default values should be as expected")
    void defaultValuesShouldBeAsExpected(){
        var provider = AutologProvider.SINGLETON;
        Assertions.assertTrue(provider.getProvidedInstance().isEmpty());
        Assertions.assertFalse(provider.getSubjectsLoggingIO());
        Assertions.assertFalse(provider.getInnerStepsLoggingIO());
        Assertions.assertEquals(IOAutologMode.CAE_NATIVE, provider.getIoAutologMode());
        Assertions.assertFalse(provider.getStructuredFormat());
        Assertions.assertFalse(provider.getLogStackTrace());
        Assertions.assertEquals(5, provider.getLinesOfStackTrace());
        Assertions.assertTrue(provider.getAsync());
    }

    @Test
    @DisplayName("Should set provided instance as expected")
    void shouldSetProvidedInstanceAsExpected(){
        var logger = Mockito.mock(Logger.class);
        var provider = AutologProvider.SINGLETON.setProvidedInstance(logger);
        Assertions.assertEquals(AutologProvider.SINGLETON, provider);
        Assertions.assertTrue(provider.getProvidedInstance().isPresent());
        Assertions.assertEquals(logger, provider.getProvidedInstance().get());
    }

    @Test
    @DisplayName("Should set use cases logging IO as expected")
    void shouldSetUseCasesLoggingIOAsExpected(){
        var provider = AutologProvider.SINGLETON.setSubjectsLoggingIO(true);
        Assertions.assertEquals(AutologProvider.SINGLETON, provider);
        Assertions.assertTrue(AutologProvider.SINGLETON.getSubjectsLoggingIO());
    }

    @Test
    @DisplayName("Should set ports logging IO as expected")
    void shouldSetPortsLoggingIOAsExpected(){
        var provider = AutologProvider.SINGLETON.setInnerStepsLoggingIO(true);
        Assertions.assertEquals(AutologProvider.SINGLETON, provider);
        Assertions.assertTrue(AutologProvider.SINGLETON.getInnerStepsLoggingIO());
    }

    @Test
    @DisplayName("Should set IO autolog mode as expected")
    void shouldSetIOAutologModeAsExpected(){
        var provider = AutologProvider.SINGLETON.setIOAutologMode(IOAutologMode.TO_STRING);
        Assertions.assertEquals(AutologProvider.SINGLETON, provider);
        Assertions.assertEquals(IOAutologMode.TO_STRING, provider.getIoAutologMode());
    }

    @Test
    @DisplayName("Should set structured format as expected")
    void shouldSetStructuredFormatAsExpected(){
        var provider = AutologProvider.SINGLETON.setStructuredFormat(true);
        Assertions.assertEquals(AutologProvider.SINGLETON, provider);
        Assertions.assertTrue(AutologProvider.SINGLETON.getStructuredFormat());
    }

    @Test
    @DisplayName("Should set logging stack trace as expected")
    void shouldSetLoggingStackTraceAsExpected(){
        var provider = AutologProvider.SINGLETON.setLoggingStackTrace(true);
        Assertions.assertEquals(AutologProvider.SINGLETON, provider);
        Assertions.assertTrue(AutologProvider.SINGLETON.getLogStackTrace());
    }

    @Test
    @DisplayName("Should set async as expected")
    void shouldSetAsyncAsExpected(){
        var provider = AutologProvider.SINGLETON.setAsync(false);
        Assertions.assertEquals(AutologProvider.SINGLETON, provider);
        Assertions.assertFalse(AutologProvider.SINGLETON.getAsync());
    }

    @Test
    @DisplayName("Should set number of lines from stack trace as expected")
    void shouldSetNumberOfLinesFromStackTraceAsExpected(){
        var provider = AutologProvider.SINGLETON.setNumberOfLinesFromStackTrace(10);
        Assertions.assertEquals(AutologProvider.SINGLETON, provider);
        Assertions.assertEquals(10, AutologProvider.SINGLETON.getLinesOfStackTrace());
    }

}
