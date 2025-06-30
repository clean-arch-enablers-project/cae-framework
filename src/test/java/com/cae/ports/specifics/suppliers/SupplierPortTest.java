package com.cae.ports.specifics.suppliers;

import com.cae.autofeatures.autolog.AutologProvider;
import com.cae.ports.SupplierPort;
import com.cae.use_cases.contexts.ExecutionContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.simulations.adapters.loggers.LoggerAdapterForTesting;
import utils.simulations.assemblers.loggers.MyAppAutologBootstrap;

import java.util.UUID;

@ExtendWith(MockitoExtension.class)
class SupplierPortTest {

    @BeforeEach
    void setup(){
        MyAppAutologBootstrap.startupDefaultSettings();
    }

    @Test
    void shouldExecuteThePortImplementationLogicAsExpected(){
        AutologProvider.SINGLETON
                .setProvidedInstance(LoggerAdapterForTesting.SINGLETON)
                .setPortsLoggingIO(true);
        var correlation = Mockito.mock(ExecutionContext.class);
        var id = UUID.randomUUID();
        Mockito.when(correlation.getCorrelationId()).thenReturn(id);
        var portImplementation = new SomeSupplierPortImplementation();
        var portImplementationResult = portImplementation.executePort(correlation);
        Mockito.verify(correlation, Mockito.times(AutologProvider.SINGLETON.getPortsLoggingIO()? 3 : 1)).getCorrelationId();
        Assertions.assertEquals(id.toString(), portImplementationResult);
    }


    public static class SomeSupplierPortImplementation extends SupplierPort<String> {
        @Override
        protected String executeLogic(ExecutionContext correlation) {
            return correlation.getCorrelationId().toString();
        }
    }

}
