package com.cae.use_cases;

import com.cae.http_client.implementations.exceptions.IORuntimeException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.SomeNormalConsumerUseCase;
import utils.SomeNormalRunnableUseCase;

import java.io.IOException;

@ExtendWith(MockitoExtension.class)
class UseCaseExecutionExceptionTest {

    @Test
    @DisplayName("Should set message as expected")
    void shouldSetMessageAsExpected(){
        var useCaseA = new SomeNormalRunnableUseCase();
        var useCaseB = new SomeNormalConsumerUseCase();
        var originalExceptionA = new RuntimeException("Some problem");
        var originalExceptionB = new IORuntimeException(new IOException("Some other problem"));
        var unitA = new UseCaseExecutionException(useCaseA, originalExceptionA);
        var unitB = new UseCaseExecutionException(useCaseB, originalExceptionB);
        var expectedBriefPublicMessageA = "Something went unexpectedly wrong while executing use case of 'some_normal_runnable'";
        var expectedDetailedMessageA = "More details on the unexpected problem: " + originalExceptionA;
        Assertions.assertEquals(expectedBriefPublicMessageA, unitA.getBriefPublicMessage());
        Assertions.assertTrue(unitA.getDetails().isPresent());
        Assertions.assertEquals(expectedDetailedMessageA, unitA.getDetails().get());
        var expectedBriefPublicMessageB = "Something went unexpectedly wrong while executing use case of 'some_normal_consumer'";
        var expectedDetailedMessageB = "More details on the unexpected problem: " + originalExceptionB;
        Assertions.assertEquals(expectedBriefPublicMessageB, unitB.getBriefPublicMessage());
        Assertions.assertTrue(unitB.getDetails().isPresent());
        Assertions.assertEquals(expectedDetailedMessageB, unitB.getDetails().get());
        Assertions.assertTrue(unitA.getOriginalException().isPresent());
        Assertions.assertEquals(originalExceptionA, unitA.getOriginalException().get());
        Assertions.assertTrue(unitB.getOriginalException().isPresent());
        Assertions.assertEquals(originalExceptionB, unitB.getOriginalException().get());
    }

}
