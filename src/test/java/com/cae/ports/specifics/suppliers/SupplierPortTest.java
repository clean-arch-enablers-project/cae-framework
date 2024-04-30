package com.cae.ports.specifics.suppliers;

import com.cae.loggers.LoggerProvider;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.simulations.adapters.loggers.LoggerAdapterForTesting;
import utils.simulations.assemblers.loggers.LoggerBootstrapForTesting;

import java.util.UUID;

@ExtendWith(MockitoExtension.class)
class SupplierPortTest {

    @BeforeEach
    void setup(){
        LoggerBootstrapForTesting.startupDefaultSettings();
    }

    @Test
    void shouldExecuteThePortImplementationLogicAsExpected(){
        LoggerProvider.SINGLETON
                .setProvidedInstance(LoggerAdapterForTesting.SINGLETON)
                .setPortsLoggingIO(true);
        var correlation = Mockito.mock(UseCaseExecutionCorrelation.class);
        var id = UUID.randomUUID();
        Mockito.when(correlation.getId()).thenReturn(id);
        var portImplementation = new SomeSupplierPortImplementation();
        var portImplementationResult = portImplementation.executePort(correlation);
        Mockito.verify(correlation, Mockito.times(LoggerProvider.SINGLETON.getPortsLoggingIO()? 2 : 1)).getId();
        Assertions.assertEquals(id.toString(), portImplementationResult);
    }


    public static class SomeSupplierPortImplementation extends SupplierPort<String>{
        @Override
        protected String executeLogic(UseCaseExecutionCorrelation correlation) {
            return correlation.getId().toString();
        }
    }

}
