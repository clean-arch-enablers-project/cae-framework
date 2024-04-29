package com.cae.ports.specifics.consumers;

import com.cae.loggers.LoggerProvider;
import com.cae.use_cases.correlations.UseCaseExecutionCorrelation;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.LoggerBootstrapForTesting;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@ExtendWith(MockitoExtension.class)
class ConsumerPortTest {

    @Mock
    private UseCaseExecutionCorrelation correlation;
    private final UUID id = UUID.randomUUID();

    @BeforeEach
    void setUp(){
        Mockito.when(this.correlation.getId()).thenReturn(this.id);
        LoggerBootstrapForTesting.startupDefaultSettings();
    }

    @Test
    void shouldExecuteThePortImplementationLogicAsExpected(){
        var portImplementation = new SomeConsumerPortImplementation();
        var stringInput = "input";
        portImplementation.executePortOn(stringInput, this.correlation);
        Mockito.verify(this.correlation, Mockito.times(LoggerProvider.SINGLETON.getPortsLoggingIO()? 2 : 1)).getId();
        var containsInputInTheListAsExpected = portImplementation.someStrings.stream().anyMatch(string -> string.equals(stringInput));
        var containsCorrelationIdInTheListAsExpected = portImplementation.someStrings.stream().anyMatch(string -> string.equals(this.correlation.getId().toString()));
        Assertions.assertTrue(containsInputInTheListAsExpected);
        Assertions.assertTrue(containsCorrelationIdInTheListAsExpected);
    }

    private static class SomeConsumerPortImplementation extends ConsumerPort<String> {
        public final List<String> someStrings = new ArrayList<>();
        @Override
        protected void executeLogic(String input, UseCaseExecutionCorrelation correlation) {
             this.someStrings.add(input);
             this.someStrings.add(correlation.getId().toString());
        }
    }

}
