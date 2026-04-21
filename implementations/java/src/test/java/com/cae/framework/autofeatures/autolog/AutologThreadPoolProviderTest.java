package com.cae.framework.autofeatures.autolog;

import com.cae.framework.autofeatures.AutofeatureThreadPoolProvider;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class AutologThreadPoolProviderTest {

    @Test
    @DisplayName("Should be instance of AutofeatureThreadPoolProvider")
    void shouldBeInstanceOfAutofeatureThreadPoolProvider(){
        Assertions.assertInstanceOf(AutofeatureThreadPoolProvider.class, AutologThreadPoolProvider.SINGLETON);
    }

}
