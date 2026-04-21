package com.cae.framework.ports;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class PortTest {

    @Test
    @DisplayName("Should set port name as expected")
    void shouldSetPortNameAsExpected(){
        var port = new SomeRandomSimpleBasicExtremelyEasyPort();
        Assertions.assertEquals("SomeRandomSimpleBasicExtremelyEasyPort", port.getName());
    }

    public static class SomeRandomSimpleBasicExtremelyEasyPort extends Port {}
    
}
