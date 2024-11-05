package com.cae.ports.specifics.runnables;

import com.cae.ports.RunnablePort;
import com.cae.use_cases.contexts.ExecutionContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.simulations.assemblers.loggers.LoggerBootstrapForTesting;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@ExtendWith(MockitoExtension.class)
class RunnablePortTest {

    @BeforeEach
    void setup(){
        LoggerBootstrapForTesting.startupDefaultSettings();
    }

    @Test
    void shouldExecuteThePortImplementationLogicAsExpected(){
        var correlation = Mockito.mock(ExecutionContext.class);
        var id = UUID.randomUUID();
        Mockito.when(correlation.getCorrelationId()).thenReturn(id);
        var portImplementation = new SomeRunnablePortImplementation();
        portImplementation.executePort(correlation);
        Mockito.verify(correlation, Mockito.times(3)).getCorrelationId();
        Assertions.assertTrue(portImplementation.someStrings.contains(id.toString()));
    }

    public static class SomeRunnablePortImplementation extends RunnablePort {
        public final List<String> someStrings = new ArrayList<>();
        @Override
        protected void executeLogic(ExecutionContext correlation) {
            this.someStrings.add(correlation.getCorrelationId().toString());
        }
    }


}
