package com.cae.use_cases.correlations;

import com.cae.use_cases.correlations.exceptions.CorrelationIdValueFormatException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.LoggerBootstrapForTesting;

import java.util.UUID;

@ExtendWith(MockitoExtension.class)
class UseCaseExecutionCorrelationTest {

    private final UUID randomUUID = UUID.randomUUID();

    @BeforeEach
    void setup(){
        LoggerBootstrapForTesting.startupDefaultSettings();
    }

    @Test
    void shouldInstantiateCorrectlyByTheStringConstructorMethod(){
        var correlation = UseCaseExecutionCorrelation.of(this.randomUUID.toString());
        Assertions.assertNotNull(correlation);
        Assertions.assertNotNull(correlation.getId());
        Assertions.assertEquals(this.randomUUID.toString(), correlation.getId().toString());
    }

    @Test
    void shouldInstantiateCorrectlyByTheDefaultConstructorMethod(){
        var correlation = new UseCaseExecutionCorrelation(this.randomUUID);
        Assertions.assertNotNull(correlation);
        Assertions.assertNotNull(correlation.getId());
        Assertions.assertEquals(this.randomUUID, correlation.getId());
    }

    @Test
    void shouldInstantiateCorrectlyByTheRandomIDConstructorMethod(){
        var correlation = UseCaseExecutionCorrelation.ofNew();
        Assertions.assertNotNull(correlation);
        Assertions.assertNotNull(correlation.getId());
    }

    @Test
    void shouldThrowExceptionWhenIdValueIsNotInUUIDFormat(){
        Assertions.assertThrows(CorrelationIdValueFormatException.class, () -> UseCaseExecutionCorrelation.of("some not UUID string"));
    }

    @Test
    void shouldReturnToStringAsExpected(){
        Assertions.assertEquals(this.randomUUID.toString(), new UseCaseExecutionCorrelation(this.randomUUID).toString());
    }

}
