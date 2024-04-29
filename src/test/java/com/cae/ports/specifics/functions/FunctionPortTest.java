package com.cae.ports.specifics.functions;

import com.cae.loggers.LoggerProvider;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.LoggerAdapterForTesting;

import java.util.UUID;

@ExtendWith(MockitoExtension.class)
class FunctionPortTest {

    @Mock
    private UseCaseExecutionCorrelation correlation;
    private final UUID id = UUID.randomUUID();

    @BeforeEach
    void setUp(){
        LoggerProvider.SINGLETON
                .setPortsLoggingIO(true)
                .setProvidedInstance(LoggerAdapterForTesting.SINGLETON);
        Mockito.when(this.correlation.getId()).thenReturn(this.id);
    }

    @Test
    void shouldExecuteThePortImplementationLogicAsExpected(){
        var portImplementation = new SomeFunctionPortImplementation();
        var stringInput = "input";
        var portResult = portImplementation.executePortOn(stringInput, this.correlation);
        Assertions.assertFalse(portResult);
        Mockito.verify(this.correlation, Mockito.times(LoggerProvider.SINGLETON.getPortsLoggingIO()? 2 : 1)).getId();
    }

    private static class SomeFunctionPortImplementation extends FunctionPort<String, Boolean>{
        @Override
        protected Boolean executeLogic(String input, UseCaseExecutionCorrelation correlation) {
            return input.isBlank() || correlation.getId().toString().isBlank();
        }
    }

}
