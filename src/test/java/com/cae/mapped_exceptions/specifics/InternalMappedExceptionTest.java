package com.cae.mapped_exceptions.specifics;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class InternalMappedExceptionTest {

    @Test
    void shouldInstantiateCorrectlyWithPublicMessagePlusDetails(){
        var briefPublicMessage = "some brief message";
        var details = "some really big and notorious details to aggregate the understanding of the exception. Blablabla, bla bla, blablabla blabla.";
        var exception = new InternalMappedException(briefPublicMessage, details);
        Assertions.assertNotNull(exception);
        Assertions.assertEquals(briefPublicMessage, exception.getBriefPublicMessage());
        Assertions.assertTrue(exception.getDetails().isPresent());
        Assertions.assertEquals(briefPublicMessage + " | " + details, exception.getMessage());
    }

}
