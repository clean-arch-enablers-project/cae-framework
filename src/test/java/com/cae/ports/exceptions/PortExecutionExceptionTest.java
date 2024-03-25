package com.cae.ports.exceptions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class PortExecutionExceptionTest {

     @Test
     void shouldBeInstantiatedWithExpectedMessages(){
          var name = "some_name";
          var brief = "Something went unexpectedly wrong while trying to execute port '" + name + "'";
          var unexpectedExceptionCaught = new RuntimeException("some stuff over here");
          var details = "More details: " + unexpectedExceptionCaught;
          var portException = new PortExecutionException(unexpectedExceptionCaught, name);
          Assertions.assertEquals(brief, portException.getBriefPublicMessage());
          Assertions.assertTrue(portException.getDetails().isPresent());
          Assertions.assertEquals(details, portException.getDetails().get());
          Assertions.assertEquals(brief + " | " + details, portException.getMessage());
     }
}
