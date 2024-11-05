package com.cae.use_cases;

import com.cae.use_cases.metadata.UseCaseMetadata;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.simulations.assemblers.loggers.LoggerBootstrapForTesting;

@ExtendWith(MockitoExtension.class)
class UseCaseExecutionExceptionTest {

    @BeforeEach
    void setup(){
        LoggerBootstrapForTesting.startupDefaultSettings();
    }

    @Test
    void shouldInstantiateExceptionAsExpected(){
        var unexpectedException = new RuntimeException("oh no!");
        var useCaseName = "AwesomeUseCase";
        var useCase = Mockito.mock(UseCase.class);
        var useCaseMetadata = Mockito.mock(UseCaseMetadata.class);
        Mockito.when(useCase.getUseCaseMetadata()).thenReturn(useCaseMetadata);
        Mockito.when(useCaseMetadata.getName()).thenReturn(useCaseName);
        var useCaseExecutionException = new UseCaseExecutionException(useCase, unexpectedException);
        var expectedBriefPublicMessage =  "Something went unexpectedly wrong while executing use case of 'AwesomeUseCase'";
        Assertions.assertEquals(expectedBriefPublicMessage, useCaseExecutionException.getBriefPublicMessage());
        Assertions.assertTrue(useCaseExecutionException.getDetails().isPresent());
        var expectedDetails = "More details on the unexpected problem: " + unexpectedException;
        Assertions.assertTrue(useCaseExecutionException.getDetails().get().contains(expectedDetails));
    }

}
