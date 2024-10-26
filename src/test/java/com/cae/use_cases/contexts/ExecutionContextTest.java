package com.cae.use_cases.contexts;

import com.cae.use_cases.contexts.exceptions.CorrelationIdValueFormatException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.simulations.assemblers.loggers.LoggerBootstrapForTesting;

import java.util.UUID;

@ExtendWith(MockitoExtension.class)
class ExecutionContextTest {

    private final UUID randomUUID = UUID.randomUUID();

    @BeforeEach
    void setup(){
        LoggerBootstrapForTesting.startupDefaultSettings();
    }

    @Test
    void shouldInstantiateCorrectlyByTheStringConstructorMethod(){
        var correlation = ExecutionContext.of(this.randomUUID.toString());
        Assertions.assertNotNull(correlation);
        Assertions.assertNotNull(correlation.getCorrelationId());
        Assertions.assertEquals(this.randomUUID.toString(), correlation.getCorrelationId().toString());
    }

    @Test
    void shouldInstantiateCorrectlyByTheDefaultConstructorMethod(){
        var correlation = new ExecutionContext(this.randomUUID);
        Assertions.assertNotNull(correlation);
        Assertions.assertNotNull(correlation.getCorrelationId());
        Assertions.assertEquals(this.randomUUID, correlation.getCorrelationId());
    }

    @Test
    void shouldInstantiateCorrectlyByTheRandomIDConstructorMethod(){
        var correlation = ExecutionContext.ofNew();
        Assertions.assertNotNull(correlation);
        Assertions.assertNotNull(correlation.getCorrelationId());
    }

    @Test
    void shouldThrowExceptionWhenIdValueIsNotInUUIDFormat(){
        Assertions.assertThrows(CorrelationIdValueFormatException.class, () -> ExecutionContext.of("some not UUID string"));
    }

    @Test
    void shouldReturnToStringAsExpected(){
        Assertions.assertEquals(this.randomUUID.toString(), new ExecutionContext(this.randomUUID).toString());
    }

}
