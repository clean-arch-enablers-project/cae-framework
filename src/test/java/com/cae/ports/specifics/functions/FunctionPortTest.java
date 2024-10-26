package com.cae.ports.specifics.functions;

import com.cae.loggers.LoggerProvider;
import com.cae.use_cases.contexts.ExecutionContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.simulations.adapters.loggers.LoggerAdapterForTesting;

import java.util.UUID;

@ExtendWith(MockitoExtension.class)
class FunctionPortTest {

    @Mock
    private ExecutionContext correlation;
    private final UUID id = UUID.randomUUID();

    @BeforeEach
    void setUp(){
        LoggerProvider.SINGLETON
                .setPortsLoggingIO(true)
                .setProvidedInstance(LoggerAdapterForTesting.SINGLETON);
        Mockito.when(this.correlation.getCorrelationId()).thenReturn(this.id);
    }

    @Test
    void shouldExecuteThePortImplementationLogicAsExpected(){
        var portImplementation = new SomeFunctionPortImplementation();
        var stringInput = "input";
        var portResult = portImplementation.executePortOn(stringInput, this.correlation);
        Assertions.assertFalse(portResult);
        Mockito.verify(this.correlation, Mockito.times(LoggerProvider.SINGLETON.getPortsLoggingIO()? 3 : 1)).getCorrelationId();
    }

    private static class SomeFunctionPortImplementation extends FunctionPort<String, Boolean>{
        @Override
        protected Boolean executeLogic(String input, ExecutionContext correlation) {
            return input.isBlank() || correlation.getCorrelationId().toString().isBlank();
        }
    }

}
