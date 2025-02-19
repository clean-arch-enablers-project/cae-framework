package com.cae.ports;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import utils.simulations.assemblers.loggers.MyAppAutologBootstrap;

@ExtendWith(MockitoExtension.class)
class PortTest {

    private static class SomePortImplementation extends Port{}

    @BeforeEach
    void setup(){
        MyAppAutologBootstrap.startupDefaultSettings();
    }

    @Test
    void shouldSetNameRight(){
        var somePortImplementation = new SomePortImplementation();
        Assertions.assertEquals(SomePortImplementation.class.getSimpleName(), somePortImplementation.getName());
    }

}
