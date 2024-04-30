package com.cae.ports.specifics.runnables;

import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
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
        var correlation = Mockito.mock(UseCaseExecutionCorrelation.class);
        var id = UUID.randomUUID();
        Mockito.when(correlation.getId()).thenReturn(id);
        var portImplementation = new SomeRunnablePortImplementation();
        portImplementation.executePort(correlation);
        Mockito.verify(correlation, Mockito.times(1)).getId();
        Assertions.assertTrue(portImplementation.someStrings.contains(id.toString()));
    }

    public static class SomeRunnablePortImplementation extends RunnablePort{
        public final List<String> someStrings = new ArrayList<>();
        @Override
        protected void executeLogic(UseCaseExecutionCorrelation correlation) {
            this.someStrings.add(correlation.getId().toString());
        }
    }


}
