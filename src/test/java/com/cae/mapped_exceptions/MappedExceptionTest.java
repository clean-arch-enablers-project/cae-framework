package com.cae.mapped_exceptions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class MappedExceptionTest {

    @Test
    void shouldInstantiateCorrectlyWithPublicMessagePlusDetails(){
        var briefPublicMessage = "some brief message";
        var details = "some really big and notorious details to aggregate the understanding of the exception. Blablabla, bla bla, blablabla blabla.";
        var exception = new MappedException(briefPublicMessage, details);
        Assertions.assertNotNull(exception);
        Assertions.assertEquals(briefPublicMessage, exception.getBriefPublicMessage());
        Assertions.assertTrue(exception.getDetails().isPresent());
        Assertions.assertEquals(briefPublicMessage + " | " + details, exception.getMessage());
    }

    @Test
    void shouldInstantiateCorrectlyWithPublicMessageButWithoutDetails(){
        var briefPublicMessage = "some brief message";
        var exception = new MappedException(briefPublicMessage);
        Assertions.assertNotNull(exception);
        Assertions.assertEquals(briefPublicMessage, exception.getBriefPublicMessage());
        Assertions.assertTrue(exception.getDetails().isEmpty());
        Assertions.assertEquals(briefPublicMessage, exception.getMessage());
    }

}
