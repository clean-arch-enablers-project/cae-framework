package com.cae.framework.autofeatures.autometrics;

import com.cae.framework.autofeatures.AutofeatureThreadPoolProvider;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class AutometricsThreadPoolProviderTest {

    @Test
    @DisplayName("Should be instance of AutofeatureThreadPoolProvider")
    void shouldBeInstanceOfAutofeatureThreadPoolProvider(){
        Assertions.assertInstanceOf(AutofeatureThreadPoolProvider.class, AutometricsThreadPoolProvider.SINGLETON);
    }

}
