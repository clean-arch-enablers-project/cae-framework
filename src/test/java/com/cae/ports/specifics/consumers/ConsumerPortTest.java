package com.cae.ports.specifics.consumers;

import com.cae.loggers.LoggerProvider;
import com.cae.use_cases.contexts.ExecutionContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.simulations.assemblers.loggers.LoggerBootstrapForTesting;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@ExtendWith(MockitoExtension.class)
class ConsumerPortTest {

    @Mock
    private ExecutionContext context;
    private final UUID id = UUID.randomUUID();

    @BeforeEach
    void setUp(){
        Mockito.when(this.context.getCorrelationId()).thenReturn(this.id);
        LoggerBootstrapForTesting.startupDefaultSettings();
    }

    @Test
    void shouldExecuteThePortImplementationLogicAsExpected(){
        var portImplementation = new SomeConsumerPortImplementation();
        var stringInput = "input";
        portImplementation.executePortOn(stringInput, this.context);
        Mockito.verify(this.context, Mockito.times(LoggerProvider.SINGLETON.getPortsLoggingIO()? 3 : 1)).getCorrelationId();
        var containsInputInTheListAsExpected = portImplementation.someStrings.stream().anyMatch(string -> string.equals(stringInput));
        var containsCorrelationIdInTheListAsExpected = portImplementation.someStrings.stream().anyMatch(string -> string.equals(this.context.getCorrelationId().toString()));
        Assertions.assertTrue(containsInputInTheListAsExpected);
        Assertions.assertTrue(containsCorrelationIdInTheListAsExpected);
    }

    private static class SomeConsumerPortImplementation extends ConsumerPort<String> {
        public final List<String> someStrings = new ArrayList<>();
        @Override
        protected void executeLogic(String input, ExecutionContext correlation) {
             this.someStrings.add(input);
             this.someStrings.add(correlation.getCorrelationId().toString());
        }
    }

}
