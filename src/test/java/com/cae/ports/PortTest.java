package com.cae.ports;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class PortTest {

    private static class SomePortImplementation extends Port{}

    @Test
    void shouldSetNameRight(){
        var somePortImplementation = new SomePortImplementation();
        Assertions.assertEquals(SomePortImplementation.class.getSimpleName(), somePortImplementation.getName());
    }

}
